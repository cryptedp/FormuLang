package ch.waan.shuntingYard

import scala.annotation.tailrec
import java.util.Scanner
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.util.Success

object ASLParser {

  private def pop(opStack: List[TOKEN], numStack: List[Node]): (List[TOKEN], List[Node]) = {
    opStack.head match {
      case o: OP       => (opStack.tail, OpNode(o, numStack.tail.head, numStack.head) :: numStack.tail.tail)
      case f: FUNCTION => (opStack.tail, FunNode(f, numStack.head) :: numStack.tail)
      case LPAR | RPAR => (opStack.tail, numStack)
    }
  }

  private def shunt(tokens: List[TOKEN]): Try[Node] = try {
    shunt(tokens, List(), List())
  } catch {
    case t: Throwable => Failure(t)
  }

  @tailrec
  private def shunt(tokens: List[TOKEN], opStack: List[TOKEN], numStack: List[Node]): Try[Node] =
    if (tokens.isEmpty) {
      var ops = opStack
      var nms = numStack
      while (!ops.isEmpty) {
        if (ops.head == LPAR)
          return Failure(new IllegalStateException("mismatched parentheses"))
        val (a, b) = pop(ops, nms)
        ops = a
        nms = b
      }
      Success(nms.head)
    } else
      tokens.head match {
        case ILLEGAL_TOKEN(s) => Failure(new IllegalStateException("Illegal Token \"" + s + "\""))
        case n: NUMBER        => shunt(tokens.tail, opStack, n :: numStack)
        case s: SYMBOLIC      => shunt(tokens.tail, opStack, s :: numStack)
        case f: FUNCTION      => shunt(tokens.tail, f :: opStack, numStack)
        case o: OP => {
          var ops = opStack
          var nms = numStack
          while (!ops.isEmpty && (ops.head match {
            case op: OP if o.isRightAssociative  => o.precedence < op.precedence
            case op: OP if !o.isRightAssociative => o.precedence <= op.precedence
            case f: FUNCTION                     => true
            case _                               => false
          })) {
            val (a, b) = pop(ops, nms)
            ops = a
            nms = b
          }
          shunt(tokens.tail, o :: ops, nms)
        }
        case LPAR => shunt(tokens.tail, LPAR :: opStack, numStack)
        case RPAR => {
          var ops = opStack
          var nms = numStack
          while (ops.head match {
            case LPAR => false
            case _    => true
          }) {
            val (a, b) = pop(ops, nms)
            ops = a
            nms = b
          }
          {
            val (a, b) = pop(ops, nms)
            ops = a
            nms = b
          }
          if (ops.headOption.filter(_.isInstanceOf[FUNCTION]).isDefined) {
            val (a, b) = pop(ops, nms)
            ops = a
            nms = b
          }
          shunt(tokens.tail, ops, nms)
        }
      }

  def process(s: String) = {
    val shunted = ASLTokenizer tokenizeString s flatMap shunt
    shunted.flatMap(s => {
      val validation = s.validate
      if (validation.isEmpty)
        Success(s)
      else
        Failure(new IllegalStateException(s"ASL validation failed: ${validation.get}"))
    })
  }

}
