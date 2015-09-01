package ch.waan.shuntingYard

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object ASLTokenizer {

  private def inflateString(s: String) = s
    .replaceAll("(\\+|\\-|\\/|\\*|\\^|\\(|\\)|\\,)(.)", "$1 $2")
    .replaceAll("(.)(\\+|\\-|\\/|\\*|\\^|\\(|\\)|\\,)", "$1 $2")

  def tokenizeString(s: String): Try[List[TOKEN]] = {
    val tokens = inflateString(inflateString(s)).split(" ").toList.filterNot("".equals).map(ASLTokenizer.tokenize)
    val validation = tokens.map(_.validate).reduce(_ orElse _)
    if (validation.isEmpty)
      Success(tokens)
    else
      Failure(new RuntimeException(s"Token validation failed: ${validation.get}"))
  }

  private[this] val num = "\\d+(\\.\\d+)?".r
  private[this] val fun = "[a-z][a-z0-9]+".r
  private[this] val sym = "[a-z]".r

  private def tokenize(s: String) = {
    s match {
      case "="     => ASSIGN
      case "+"     => PLUS
      case "-"     => MINUS
      case "/"     => DIV
      case "*"     => MUL
      case "^"     => POW
      case "("     => LPAR
      case ")"     => RPAR
      case ","     => ARSP
      case num(_*) => NUMBER(s.toDouble)
      case fun(_*) => FUNCTION(s)
      case sym(_*) => SYMBOLIC(s)
      case wtf     => ILLEGAL_TOKEN(wtf)
    }
  }

}

trait Validatable {
  def validate: Option[String]
}
trait CertainValidation extends Validatable {
  override def validate = Option.empty
}

trait TOKEN extends Validatable
trait Node extends Validatable {
  def insertSymbolic(s: String, value: Double): Node
}

case class ListNode(elements: List[Node]) extends Node {
  // lists are valid when they are not empty and all elements are valid
  def validate = if (elements.isEmpty) Option("empty list") else elements.map(_.validate).reduce(_ orElse _)
  def insertSymbolic(s: String, v: Double) = ListNode(elements.map(_.insertSymbolic(s, v)))
}

case class OpNode(op: OP, arg0: Node, arg1: Node) extends Node {
  def insertSymbolic(s: String, v: Double) = op match {
    case ASSIGN => OpNode(ASSIGN, arg0, arg1.insertSymbolic(s, v))
    case _      => OpNode(op, arg0.insertSymbolic(s, v), arg1.insertSymbolic(s, v))
  }
  def validate = (op, arg0, arg1) match {
    case (ASSIGN, SYMBOLIC(s), b)   => b.validate
    case (ASSIGN, _, _)             => Option("invalid assignment. Reference must be a symbol")
    case (ARSP, a, b)               => a.validate orElse b.validate
    case (_, OpNode(ARSP, _, _), _) => Option("unexpected argument separator")
    case (_, _, OpNode(ARSP, _, _)) => Option("unexpected argument separator")
    case (_, a, b)                  => a.validate orElse b.validate
  }
}
case class FunNode(f: FUNCTION, args: Node) extends Node {
  def insertSymbolic(s: String, v: Double) = FunNode(f, args.insertSymbolic(s, v))
  def validate = args.validate
}
case class NUMBER(value: Double) extends TOKEN with Node {
  def insertSymbolic(s: String, v: Double) = this
  def validate = Option.empty
}

case object LPAR extends TOKEN with CertainValidation
case object RPAR extends TOKEN with CertainValidation
case class FUNCTION(name: String) extends TOKEN with CertainValidation

sealed class OP(val precedence: Int, val isRightAssociative: Boolean = false) extends TOKEN with CertainValidation
case object ARSP extends OP(1)
case object PLUS extends OP(2)
case object MINUS extends OP(2)
case object DIV extends OP(3)
case object MUL extends OP(3)
case object POW extends OP(4, true)
case object ASSIGN extends OP(1)

case class SYMBOLIC(s: String) extends TOKEN with Node {
  def insertSymbolic(sy: String, v: Double) = if (sy equals s) NUMBER(v) else this
  def validate = Option.empty
}

case class ILLEGAL_TOKEN(s: String) extends TOKEN {
  def validate = Option(s"illegal token: $s")
}