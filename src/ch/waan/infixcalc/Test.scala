package ch.waan.infixcalc

import java.io.StringReader

object Test extends App {

  val input = "writeX(#real) = x => y = y + 1"

  val r = new StringReader(input)

  val precompile =
    Precompiler.tokenize andThen
      Precompiler.clumpTokens andThen
      Precompiler.translateRaw andThen
      Precompiler.inferFunCalls andThen
      Treeprocessor.shuntToTree andThen
      Treeprocessor.ripenTrees andThen
      Compiledown.treeToPostfix

  println(precompile(r).toList)

}