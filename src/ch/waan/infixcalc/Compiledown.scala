package ch.waan.infixcalc

import ch.waan.infixcalc.Tree.ASSIGN
import scala.collection.mutable

sealed trait OP
object OP {
  case class LITERAL_STRING(s: String) extends OP
  case class LITERAL_NUMBER(v: Double) extends OP
  case class LITERAL_TYPE(t: String) extends OP
  case class LITERAL_SYMBOL(s: String) extends OP
  case class LITERAL_CODE(ops: List[OP]) extends OP
  case object TYPEOF extends OP
  case object EVAL extends OP
  case object ASSIGN extends OP
  case object GETVAL extends OP
  case object PUSH extends OP
  case object POP extends OP
  case object COPY extends OP
  case object SWAP extends OP
}

object Compiledown {
  type Trav[+T] = TraversableOnce[T]

  private def compileTree(t: Tree): List[OP] = t match {
    case Tree.Symbol(s)                => List(OP.LITERAL_SYMBOL(s))
    case Tree.Type(t)                  => List(OP.LITERAL_TYPE(t))
    case Tree.Number(x)                => List(OP.LITERAL_NUMBER(x))
    case Tree.Text(t)                  => List(OP.LITERAL_STRING(t))
    case Tree.GETVAL(t)                => compileTree(t) ::: List(OP.GETVAL)
    case Tree.EVAL(s, Tree.PACK(list)) => compileTree(s) ::: list.flatMap(compileTree) ::: List(OP.LITERAL_NUMBER(list.size), OP.EVAL)
    case Tree.ASSIGN(s, v)             => compileTree(v) ::: compileTree(s) ::: List(OP.ASSIGN)
    case Tree.TYPEOF(t)                => compileTree(t) ::: List(OP.TYPEOF)
    case lambda: Tree.LAMBDA           => compileLambda(lambda)
  }

  private def insertStackManagement(code: List[OP]): List[OP] = {
    var opened = mutable.HashMap[String, Int]()
    var reversed = List[OP]()
    var i = 0
    for (op <- code) {
      i += 1
      (op, reversed.headOption) match {
        case (OP.ASSIGN, Some(OP.LITERAL_SYMBOL(s))) if !opened.contains(s) =>
          opened(s) = i
          i += 2
          reversed = OP.ASSIGN :: OP.PUSH :: OP.COPY :: reversed
        case (command, _) => reversed = command :: reversed
      }
    }

    val reversedLength = reversed.size

    var closed = List[String]()
    var result = List[OP]()
    var j = reversedLength
    while (!reversed.isEmpty) {
      j -= 1
      reversed.headOption match {
        case Some(OP.GETVAL) =>
          reversed.tail.headOption match {
            case Some(OP.LITERAL_SYMBOL(s)) =>
              if (opened.contains(s) && opened(s) > j)
                throw new IllegalStateException("var used before declaration: " + s)
              closed = s :: closed
              result = OP.LITERAL_SYMBOL(s) :: OP.COPY :: OP.GETVAL :: OP.SWAP :: OP.POP :: result
              reversed = reversed.tail
            case _ => result = OP.GETVAL :: result
          }
          reversed = reversed.tail
        case Some(command) =>
          result = command :: result
          reversed = reversed.tail
        case None => ???
      }
    }

    if ((closed diff opened.keys.toList).size != 0)
      throw new IllegalStateException("symbols never got opened, but were closed: " + (closed diff opened.keys.toList))

    val unclosed = opened.keys.toList diff closed

    var temp = List[OP]()

    while (!result.isEmpty) {
      result match {
        case OP.LITERAL_SYMBOL(s) :: OP.COPY :: OP.PUSH :: OP.ASSIGN :: tail if unclosed contains s =>
          temp = OP.POP :: OP.LITERAL_SYMBOL(s) :: OP.ASSIGN :: OP.PUSH :: OP.COPY :: OP.LITERAL_SYMBOL(s) :: temp
          result = tail
        case command :: tail =>
          temp = command :: temp
          result = tail
      }
    }

    temp.reverse
  }

  private def compileLambda(lambda: Tree.LAMBDA) =
    List(OP.LITERAL_CODE(insertStackManagement(lambda.params.reverse.flatMap(symbol => List(OP.LITERAL_SYMBOL(symbol.symbol), OP.ASSIGN)) ::: compileTree(lambda.code))))

  val treeToPostfix: Trav[Tree] => Trav[List[OP]] = _ map compileTree
}