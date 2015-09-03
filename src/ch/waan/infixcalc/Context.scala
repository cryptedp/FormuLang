package ch.waan.infixcalc
/*
class Context {

  private var tokenizer = new Tokenizer(List())
  private var tokenParser = new TokenParser(List())

  private var tokenizeRules: List[PartialFunction[String, Boolean]] = List()
  private var wordProcessingRules: List[PartialFunction[String, Word]] = List()

  def addTokenizeRule(rule: PartialFunction[String, Boolean]) = {
    tokenizeRules = rule :: tokenizeRules
    tokenizer = new Tokenizer(tokenizeRules)
  }

  def addWordProcessingRule(rule: PartialFunction[String, Word]) = {
    wordProcessingRules = rule :: wordProcessingRules
    tokenParser = new TokenParser(wordProcessingRules)
  }

  def getTokenizer = tokenizer
  def getTokenParser = tokenParser

}

object Context {

  def defaultContext = {
    val context = new Context()

    val numberRg = "^(\\d+(\\.\\d+)?)$".r
    val symbolRg = "^([a-zA-Z][A-Za-z_0-9]*)$".r
    val typeRg = "^(\\#[a-z][a-z_0-9]*)$".r

    context addTokenizeRule {
      // reassignment operators
      case "+=" | "-=" | "*=" | "/=" | "%=" | "^=" => true
      // comparisons
      case "==" | ">=" | "<=" | "!=" | "<" | ">"   => true
      // normal operators
      case "+" | "-" | "*" | "/" | "%" | "^"       => true
      // assignment and special operators
      case "=" | "." | ":" | "," | "(" | ")"       => true
      // symbols
      case numberRg(_*) => true
    }

    context addWordProcessingRule {
      // container types
      case numberRg(x, _*) => ConstantWord(x.toDouble)
      case symbolRg(s, _*) => SymbolicWord(Symbol(s))
      // terminator
      case ";"             => EOLWord
      // type-checking
      case ":"             => OpWord(":", 8)
      case "."             => OpWord(".", 8)
      case "=>"            => OpWord("=>", 2)
      // parentheses & list assembly
      case "("             => LeftPAR
      case ")"             => RightPAR
      case ","             => OpWord(",", 1)
      // operations
      case "+"             => OpWord("+", 4)
      case "-"             => OpWord("-", 4)
      case "*"             => OpWord("*", 5)
      case "/"             => OpWord("/", 5)
      case "%"             => OpWord("/", 5)
      case "^"             => OpWord("^", 6, true)
      // comparisons
      case "<"             => OpWord("<", 3)
      case ">"             => OpWord(">", 3)
      case ">="            => OpWord(">=", 3)
      case "<="            => OpWord("<=", 3)
      case "=="            => OpWord("==", 3)
      case "!="            => OpWord("!=", 3)
      // assignment
      case "="             => OpWord("=", 2, true)
      case "+="            => OpWord("+=", 2, true)
      case "-="            => OpWord("-=", 2, true)
      case "*="            => OpWord("*=", 2, true)
      case "/="            => OpWord("/=", 2, true)
      case "%="            => OpWord("%=", 2, true)
      case "^="            => OpWord("^=", 2, true)
    }

    context
  }
} */