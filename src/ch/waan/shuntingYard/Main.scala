package ch.waan.shuntingYard

import java.util.Scanner
import scala.util.Success
import scala.util.Failure

object Main extends App {

  val universe = new Universe
  val processor = new ASLProcessor(universe)

  ioLoop

  def ioLoop = {
    val s = new Scanner(System.in)
    var input = s.nextLine
    while (!(input equals ":quit")) {
      processor evaluate input match {
        case Success(v) => println(s"$v")
        case Failure(e) => println(s"error: ${
          e match {
            case _: IllegalStateException => e.getMessage
            case _                        => e.toString
          }
        }")
      }
      input = s.nextLine
    }
  }

}