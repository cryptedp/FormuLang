package ch.waan.infixcalc

case class Module(opWorker: PartialFunction[(Data, Data, OpWord), Data])

object BasicOpModule extends Module({
  case (RealNumber(x), RealNumber(y), OpWord("+", _, _))  => RealNumber(x + y)
  case (RealNumber(x), RealNumber(y), OpWord("-", _, _))  => RealNumber(x - y)
  case (RealNumber(x), RealNumber(y), OpWord("*", _, _))  => RealNumber(x * y)
  case (RealNumber(x), RealNumber(0), OpWord("/", _, _))  => Error("Div by Zero")
  case (RealNumber(x), RealNumber(y), OpWord("/", _, _))  => RealNumber(x / y)
  case (RealNumber(x), RealNumber(y), OpWord("^", _, _))  => RealNumber(Math.pow(x, y))
  case (RealNumber(x), RealNumber(y), OpWord("<", _, _))  => RealNumber(if (x < y) 1 else 0)
  case (RealNumber(x), RealNumber(y), OpWord(">", _, _))  => RealNumber(if (x > y) 1 else 0)
  case (RealNumber(x), RealNumber(y), OpWord("<=", _, _)) => RealNumber(if (x <= y) 1 else 0)
  case (RealNumber(x), RealNumber(y), OpWord(">=", _, _)) => RealNumber(if (x >= y) 1 else 0)
  case (RealNumber(x), RealNumber(y), OpWord("==", _, _)) => RealNumber(if (x == y) 1 else 0)
  case (RealNumber(x), RealNumber(y), OpWord("!=", _, _)) => RealNumber(if (x != y) 1 else 0)
})