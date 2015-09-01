package ch.waan.infixcalc

trait Data

case class Error(value: String) extends Data {
  override def toString = s"Error: $value"
}

case class SymbolicReference(value: Symbol) extends Data {
  override def toString = "$" + value.name
}

case class RealNumber(value: Double) extends Data {
  override def toString = s"$value"
}