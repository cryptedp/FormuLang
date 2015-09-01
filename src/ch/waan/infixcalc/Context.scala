package ch.waan.infixcalc

class Context {

  private var tokenizer = new Tokenizer(List())
  private var tokenParser = new TokenParser(List())

  private var tokenizeRules: List[String] = List()
  private var wordProcessingRules: List[PartialFunction[String, Word]] = List()

  def addTokenizeRule(rule: String) = {
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

    context addTokenizeRule "\\d+(\\.\\d+)?\\D" // (decimal number)
    context addTokenizeRule "\\([^\\(]" // left parenthesis
    context addTokenizeRule "\\([^\\)]" // right parenthesis
    context addTokenizeRule ",[^,]" // comma
    context addTokenizeRule "[\\+\\-\\*\\/\\^]." // operators
    context addTokenizeRule "\\$?[a-z]+[^a-z]" // method and variable names
    context addTokenizeRule "[A-Z]+[^A-Z]" // constant names
    context addTokenizeRule "\\=\\=?[^\\=]" // assignment and equals operator
    context addTokenizeRule "(\\<|\\>)\\=?[^\\=]" // comparators
    context addTokenizeRule "\\!\\=." // not-equals

    val digitRegex = "(\\d+(\\.\\d+)?)".r
    val fieldRegex = "\\$([a-z]+|[A-Z]+)".r
    val functionRegex = "[a-z]+".r

    context addWordProcessingRule {
      // container types
      case digitRegex(x, _*)    => ConstantWord(RealNumber(x.toDouble))
      case fieldRegex(s, _*)    => ConstantWord(SymbolicReference(Symbol(s)))
      case functionRegex(s, _*) => FunctionWord(s)
      // parentheses & list assembly
      case "("                  => LeftPAR
      case ")"                  => RightPAR
      case ","                  => OpWord(",", 0)
      // operations
      case "+"                  => OpWord("+", 3)
      case "-"                  => OpWord("-", 3)
      case "*"                  => OpWord("*", 4)
      case "/"                  => OpWord("/", 4)
      case "^"                  => OpWord("^", 5, true)
      // comparisons
      case "<"                  => OpWord("<", 2)
      case ">"                  => OpWord(">", 2)
      case ">="                 => OpWord(">=", 2)
      case "<="                 => OpWord("<=", 2)
      case "=="                 => OpWord("==", 2)
      case "!="                 => OpWord("!=", 2)
      // assignment
      case "="                  => OpWord("=", 1)
    }

    context
  }
}