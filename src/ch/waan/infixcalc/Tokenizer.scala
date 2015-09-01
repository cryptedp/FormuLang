package ch.waan.infixcalc

import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.annotation.tailrec

/**
 * A Tokenizer converts an input string into a sequence of tokens.
 *
 * The tokenizer scans the input and for each character it checks
 * if any of the provided breakoff rules hits. At that point the
 * string up to the hitting-point will be converted into a token.
 * The last character will not be part of the token and will be
 * considered again for the next token. Whitespace after a token
 * is always ignored when searching for the next token. To ease
 * tokenisation, an additional space will be added to the end of
 * the input string.
 *
 * @param breakoffRules a set of rules that determines the breakoff
 *                          points for tokens
 */
class Tokenizer(breakoffRules: List[String]) {

  private[this] val compiledRules = breakoffRules.map(Pattern.compile)

  private[this] def isToken(string: String): Boolean = compiledRules.find(_.matcher(string).matches()).isDefined

  private[this] def nextToken(string: String): Option[String] = {
    var i = 1
    while (i < string.length) {
      val substring = string.substring(0, i)
      if (isToken(substring + string.charAt(i)))
        return Option(substring)
      i = i + 1
    }
    Option.empty
  }

  def tokenize(string: String): Option[List[String]] = tokenize(string, List()).map(_.reverse)

  @tailrec
  private[this] def tokenize(string: String, previousTokens: List[String]): Option[List[String]] = {
    val str = string.trim + " "
    if (str.equals(" "))
      return Option(previousTokens)
    val token = nextToken(str)
    if (token.isEmpty)
      Option.empty
    else {
      tokenize(str.substring(token.get.length), token.get :: previousTokens)
    }
  }

}

