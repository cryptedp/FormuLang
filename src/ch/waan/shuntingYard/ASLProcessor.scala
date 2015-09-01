package ch.waan.shuntingYard

import scala.util.Try
import scala.annotation.tailrec
import scala.util.Success
import scala.util.Failure

class ASLProcessor(universe: Universe) {

  def evaluate(s: String): Try[Double] = try {
    ASLParser process s map resolveSymbolics map evaluate
  } catch {
    case e: Throwable => Failure(e)
  }

  // filling in of all known symbols
  def resolveSymbolics(node: Node) = universe.symbolics.foldLeft(node)((n, t) => n.insertSymbolic(t._1, t._2))

  def evaluate(node: Node): Double = node match {
    case NUMBER(x)           => x
    case SYMBOLIC(s)         => throw new IllegalStateException(s"undefined symbol: $s")
    case OpNode(PLUS, a, b)  => evaluate(a) + evaluate(b)
    case OpNode(MINUS, a, b) => evaluate(a) - evaluate(b)
    case OpNode(MUL, a, b)   => evaluate(a) * evaluate(b)
    case OpNode(DIV, a, b)   => evaluate(a) / evaluate(b)
    case OpNode(POW, a, b)   => Math.pow(evaluate(a), evaluate(b))
    case OpNode(ASSIGN, SYMBOLIC(x), b) =>
      var value = evaluate(b)
      universe.symbolic(x, value)
      value
  }

}