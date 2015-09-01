package ch.waan.infixcalc

import scala.util.Success
import scala.util.Try
import scala.util.Failure

trait Word
trait Precedence extends Word {
  val precedence: Int
  val isRightAssoc: Boolean
}

case class ConstantWord(value: Data) extends Word
case class FunctionWord(value: String) extends Word with Precedence {
  val precedence = 6
  val isRightAssoc = false
}
case object LeftPAR extends Word with Precedence {
  val precedence = 6
  val isRightAssoc = false
}
case object RightPAR extends Word
case class OpWord(token: String, precedence: Int, isRightAssoc: Boolean = false) extends Word with Precedence

class TokenParser private[this] (private[this] val builder: PartialFunction[String, Word]) {

  def this(functions: List[PartialFunction[String, Word]]) = this(functions.reduceOption(_ orElse _).getOrElse(PartialFunction.empty))

  def parseToken(token: String): Try[Word] = Try(token).map(builder)

  def parseTokens(tokens: List[String]): Try[List[Word]] = {
    val rawlist = tokens map parseToken
    val failure = rawlist find (_.isFailure)
    if (failure.isDefined)
      return Failure(failure.get.failed.get)
    return Success(rawlist map (_.get))
  }
}