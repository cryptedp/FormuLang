package ch.waan.infixcalc

import java.io.StringReader

object Test extends App {

  val input = """sum = (x, y) => {
    x + y
    }
    z = sum(1, 2)
    """

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