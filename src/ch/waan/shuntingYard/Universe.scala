package ch.waan.shuntingYard

import scala.collection.mutable.HashMap

class Universe {

  private[this] val _symbolics = HashMap[String, Double]()

  def symbolic(s: String, v: Double) = _symbolics(s) = v
  def symbolics = _symbolics.toList

}