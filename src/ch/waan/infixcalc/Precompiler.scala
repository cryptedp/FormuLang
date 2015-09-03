package ch.waan.infixcalc

import java.io.Reader
import java.io.StreamTokenizer

// ========= PRECOMPILER ============

sealed trait Token
object Token {
  case object EOL extends Token
  case object EOF extends Token
  case class WORD(value: String) extends Token
  case class NUMBER(value: Double) extends Token
  case class SPECIAL(value: String) extends Token
  case class STRING(value: String) extends Token
}

sealed trait Word
sealed trait WordPrecedence extends Word { val prec: Int; val assoc: Boolean }
object Word {
  case object EOL extends Word
  case object LPar extends Word with WordPrecedence {
    val prec = 0; val assoc = false
  }
  case object RPar extends Word
  case class Op(s: String, prec: Int, assoc: Boolean = false) extends Word with WordPrecedence
  case class Symbolic(s: String) extends Word
  case class Text(s: String) extends Word
  case class Number(v: Double) extends Word
  case class Type(s: String) extends Word
  case object BLOCK_START extends Word
  case object BLOCK_END extends Word
}

object Precompiler {
  type Trav[+E] = TraversableOnce[E]

  /**
   * Tokenizes the input from a reader
   */
  val tokenize: Reader => Trav[Token] = reader => new Iterator[Token] {
    private val t = new StreamTokenizer(reader)
    t.resetSyntax()
    t.wordChars('a', 'z');
    t.wordChars('A', 'Z');
    t.wordChars(128 + 32, 255);
    t.wordChars('_', '_')
    t.wordChars('#', '#')
    t.whitespaceChars(0, ' ');
    t.commentChar(';');
    t.quoteChar('"');
    t.quoteChar('\'');
    t.parseNumbers();
    t.eolIsSignificant(true)
    t.slashStarComments(true)
    t.slashSlashComments(true)
    private var nxt = true
    def hasNext = nxt
    def next() = t.nextToken match {
      case StreamTokenizer.TT_EOF =>
        nxt = false
        Token.EOF
      case StreamTokenizer.TT_EOL    => Token.EOL
      case StreamTokenizer.TT_NUMBER => Token.NUMBER(t.nval)
      case StreamTokenizer.TT_WORD   => Token.WORD(t.sval)
      case x if x.toChar == '"'      => Token.STRING(t.sval)
      case x                         => Token.SPECIAL(x.toChar.toString)
    }
  }

  /**
   * Combines neighbouring special tokens into a single one
   */
  val clumpTokens: Trav[Token] => Trav[Token] = tkns => {
    var cache = Option.empty[String]
    for {
      rt <- tkns
      t <- rt match {
        case Token.SPECIAL(";") => List(Token.EOL)
        case Token.SPECIAL(x) if !x.equals(")") && !x.equals("(") && !x.equals("{") && !x.equals("}") =>
          cache = cache.map(_ + x).orElse(Option(x))
          List()
        case token if cache.isDefined =>
          var t = cache.get
          cache = Option.empty
          List(Token.SPECIAL(t), token)
        case token => List(token)
      }
    } yield t
  }

  private[this] def buildOpWord(s: String) = s match {
    case "," => Word.Op(",", 1, false)
    case "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "=>" => Word.Op(s, 2, true)
    case "==" | "!=" | "<=" | ">=" | "<" | ">" => Word.Op(s, 3)
    case "+" | "-" => Word.Op(s, 4)
    case "*" | "/" | "%" => Word.Op(s, 5)
    case "^" => Word.Op("^", 6, true)
    case ":" => Word.Op(":", 8, true)
  }

  val translateRaw: Trav[Token] => Trav[Word] =
    _ map {
      _ match {
        case Token.EOL                              => Word.EOL
        case Token.EOF                              => Word.EOL
        case Token.SPECIAL("(")                     => Word.LPar
        case Token.SPECIAL(")")                     => Word.RPar
        case Token.SPECIAL("{")                     => Word.BLOCK_START
        case Token.SPECIAL("}")                     => Word.BLOCK_END
        case Token.SPECIAL(opc)                     => buildOpWord(opc)
        case Token.NUMBER(x)                        => Word.Number(x)
        case Token.STRING(txt)                      => Word.Text(txt)
        case Token.WORD(wrd) if wrd.startsWith("#") => Word.Type(wrd)
        case Token.WORD(wrd)                        => Word.Symbolic(wrd)
      }
    }

  val inferFunCalls: Trav[Word] => Trav[Word] = wrds => {
    var cache = Option.empty[Word.Symbolic]
    for {
      rw <- wrds
      w <- rw match {
        case s: Word.Symbolic if cache.isDefined =>
          val prev = cache.get
          cache = Option(s)
          List(prev, Word.Op("FC", 7, true))
        case s: Word.Symbolic if cache.isEmpty =>
          cache = Option(s)
          List()
        case Word.LPar
          | Word.Number(_)
          | Word.Text(_)
          | Word.Type(_) if cache.isDefined =>
          val prev = cache.get
          cache = Option.empty
          List(prev, Word.Op("FC", 7, true), rw)
        case word if cache.isDefined =>
          val prev = cache.get
          cache = Option.empty
          List(prev, word)
        case word if cache.isEmpty =>
          List(word)
      }
    } yield w
  }

}