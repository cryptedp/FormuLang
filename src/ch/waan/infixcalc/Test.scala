package ch.waan.infixcalc

object Test extends App {

  val input = "deftype complex (#real, #real)"

  val context = Context.defaultContext

  val tokenizer = context.getTokenizer
  val wdrparser = context.getTokenParser

  println {
    tokenizer.tokenize(input).map(wdrparser.parseTokens)
  }
}