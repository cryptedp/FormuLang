package ch.waan.infixcalc

import scala.annotation.tailrec

sealed trait Tree
object Tree {
  case class Operation(op: String, left: Tree, right: Tree) extends Tree {
    // override def toString = s"$left $right $op"
  }
  case class Number(value: Double) extends Tree {
    //  override def toString = s"$value"
  }
  case class Type(typ: String) extends Tree {
    //  override def toString = typ
  }
  case class Text(text: String) extends Tree {
    //  override def toString = "\"" + text + "\""
  }
  case class Symbol(symbol: String) extends Tree {
    //  override def toString = symbol
  }
  case class ASSIGN(target: Symbol, value: Tree) extends Tree {
    // override def toString = s"$target $value ASSIGN"
  }
  case class GETVAL(target: Tree) extends Tree {
    //override def toString = s"$target GETVAL"
  }
  case class PACK(values: List[Tree]) extends Tree {
    // override def toString = values.map(_.toString).reduceOption(_ + " " + _).getOrElse("") + " " + values.size + " PACK"
  }
  case class EVAL(symbol: Symbol, parameters: Tree) extends Tree {
    //override def toString = symbol.toString + " " + parameters.toString + " EVAL"
  }
  case class TYPEOF(value: Tree) extends Tree {
    // override def toString = value.toString + " TYPEOF"
  }
  case class LAMBDA(params: List[Symbol], code: Tree) extends Tree
  case class BLOCK(parts: List[Tree]) extends Tree
}

object Treeprocessor {

  type Trav[+A] = TraversableOnce[A]

  class ShuntingYard(var output: List[Tree] = List(), var stack: List[Word with WordPrecedence] = List(Word.LPar)) {
    private var blockStack = List[List[Tree]]()
    // adds an element to the output
    private def publish(w: Word) {
      println(output, w)
      w match {
        case Word.BLOCK_START =>
          blockStack = output :: blockStack
          output = List()
        case Word.BLOCK_END =>
          val block = Tree.BLOCK(output.reverse)
          output = block :: blockStack.head
          blockStack = blockStack.tail
        // constants
        case Word.Number(x)   => output = Tree.Number(x) :: output
        case Word.Text(t)     => output = Tree.Text(t) :: output
        case Word.Symbolic(s) => output = Tree.Symbol(s) :: output
        case Word.Type(t)     => output = Tree.Type(t) :: output
        // operations
        case Word.Op(op, _, _) =>
          if (output.size < 2)
            throw new IllegalStateException("not enough arguments for OP: " + op)
          output = Tree.Operation(op, output.tail.head, output.head) :: output.tail.tail
        case word =>
      }
    }

    private def pop {
      if (stack.isEmpty)
        throw new IllegalStateException("no pending operations on stack")
      publish(stack.head)
      stack = stack.tail
    }

    @tailrec
    private def popToLPar {
      if (stack.isEmpty)
        throw new IllegalStateException("LPar not found")
      stack.head match {
        case Word.LPar => pop
        case _ =>
          pop
          popToLPar
      }
    }

    private def push(op: Word with WordPrecedence) {
      stack = op :: stack
    }

    @tailrec
    private def popHigherThan(op: Word with WordPrecedence) {
      if (!stack.isEmpty) {
        if ((stack.head.prec > op.prec) || (!op.assoc && stack.head.prec == op.prec)) {
          pop
          popHigherThan(op)
        }
      }
    }

    private def popAll {
      popToLPar
      push(Word.LPar)
    }

    def process(w: Word) {
      w match {
        case Word.BLOCK_START =>
          publish(w)
          push(Word.LPar)
        case Word.BLOCK_END =>
          popToLPar
          publish(w)
        case Word.Number(_)   => publish(w)
        case Word.Symbolic(_) => publish(w)
        case Word.Text(_)     => publish(w)
        case Word.Type(_)     => publish(w)
        case Word.LPar        => push(Word.LPar)
        case Word.RPar        => popToLPar
        case Word.EOL         => popAll
        case op: Word.Op =>
          popHigherThan(op)
          push(op)
      }
    }

  }

  val shuntToTree: Trav[Word] => Trav[Tree] = words => {
    val processor = new ShuntingYard
    words foreach processor.process
    // enforce completion
    processor process Word.EOL
    processor.output.reverse
  }

  val ripenTrees: Trav[Tree] => Trav[Tree] = _ map ripenTree

  private def flattenCommaOp(c: Tree): List[Tree] = c match {
    case Tree.Operation(",", a, b) => a :: flattenCommaOp(b)
    case tree                      => List(tree)
  }

  private def ripenTree(tree: Tree): Tree = tree match {

    case Tree.Operation("=", Tree.Operation("FC", Tree.Symbol(f), types), Tree.Operation("=>", params, code)) =>
      val typs = flattenCommaOp(types).asInstanceOf[List[Tree.Type]]
      val signature = f + "'" + typs.map(_.typ).reduceOption(_ + _).getOrElse("")
      val pars = flattenCommaOp(params).asInstanceOf[List[Tree.Symbol]]
      Tree.ASSIGN(Tree.Symbol(signature), Tree.LAMBDA(pars, ripenTree(code)))

    case Tree.Operation("=>", params, code) =>
      val pars = flattenCommaOp(params).asInstanceOf[List[Tree.Symbol]]
      Tree.LAMBDA(pars, ripenTree(code))

    case Tree.BLOCK(list)                                   => Tree.BLOCK(list map ripenTree)
    case Tree.Operation("=", Tree.Symbol(s), v)             => Tree.ASSIGN(Tree.Symbol(s), ripenTree(v))
    case Tree.Operation("FC", Tree.Symbol("typeof"), param) => Tree.TYPEOF(ripenTree(param))
    case Tree.Operation("FC", Tree.Symbol(s), params) =>
      Tree.EVAL(Tree.Symbol(s), ripenTree(params) match {
        case pack: Tree.PACK => pack
        case tree            => Tree.PACK(List(params))
      })
    case Tree.Operation(",", l, r) => Tree.PACK(ripenTree(l) :: (ripenTree(r) match {
      case Tree.PACK(list) => list
      case tree            => List(tree)
    }))
    case Tree.Operation("+=", Tree.Symbol(s), v) => Tree.ASSIGN(Tree.Symbol(s), ripenTree(Tree.Operation("+", Tree.Symbol(s), v)))
    case Tree.Operation("-=", Tree.Symbol(s), v) => Tree.ASSIGN(Tree.Symbol(s), ripenTree(Tree.Operation("-", Tree.Symbol(s), v)))
    case Tree.Operation("*=", Tree.Symbol(s), v) => Tree.ASSIGN(Tree.Symbol(s), ripenTree(Tree.Operation("*", Tree.Symbol(s), v)))
    case Tree.Operation("/=", Tree.Symbol(s), v) => Tree.ASSIGN(Tree.Symbol(s), ripenTree(Tree.Operation("/", Tree.Symbol(s), v)))
    case Tree.Operation("%=", Tree.Symbol(s), v) => Tree.ASSIGN(Tree.Symbol(s), ripenTree(Tree.Operation("%", Tree.Symbol(s), v)))
    case Tree.Operation("^=", Tree.Symbol(s), v) => Tree.ASSIGN(Tree.Symbol(s), ripenTree(Tree.Operation("^", Tree.Symbol(s), v)))
    case Tree.Operation(op, l, r)                => Tree.EVAL(Tree.Symbol(op), Tree.PACK(List(ripenTree(l), ripenTree(r))))
    case Tree.Symbol(s)                          => Tree.GETVAL(Tree.Symbol(s))
    case tree                                    => tree
  }

}