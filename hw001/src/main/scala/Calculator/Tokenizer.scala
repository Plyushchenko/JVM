package Calculator

import scala.collection.mutable.ListBuffer

private[Calculator] object Tokenizer {
  /**
    * Split input into tokens(functions/operations/numbers/constants) and number values using
    * regular expressions
    * @param s Arithmetic expression represented as String
    * @return Tuple containing two lists, first list containing tokens, second list containing
    *         number values
    */
  def extractTokensAndNumbers(s: String): (ListBuffer[Token], ListBuffer[Double]) = {
    val tokens = new ListBuffer[Token]
    val numbers = new ListBuffer[Double]
    var input = s.replaceAll("\\s", "").toLowerCase
    val regexToToken = List(
      ("""\(""".r, LeftBracket),
      ("""\)""".r, RightBracket),
      ("""\,""".r, Comma),
      ("""\+""".r, Addition),
      ("""\-""".r, Subtraction),
      ("""\*""".r, Multiplication),
      ("""\/""".r, Division),
      ("""\^""".r, Exponentiation),
      ("""sin""".r, Sinus),
      ("""cos""".r, Cosinus),
      ("""pow""".r, Power),
      ("""pi""".r, Pi),
      ("""(0|([1-9][0-9]*))(\.[0-9]+)?""".r, Number)
    )
    while (input.nonEmpty) {
      val someRegexToken = regexToToken.find(x => x._1.findPrefixOf(input).isDefined)
      someRegexToken match {
        case Some((regex, token)) =>
          val prefixToken = regex.findPrefixOf(input).get
          token match {
            case Number =>
              numbers += prefixToken.toDouble
              tokens += token
            case Subtraction =>
              if (tokens.isEmpty || tokens.last == LeftBracket || tokens.last == Comma) {
                tokens += UnaryMinus
              } else {
                tokens += token
              }
            case _ => tokens += token
          }
          input = input.substring(prefixToken.length)
        case None =>
          throw UnknownTokenException
      }
    }
    (tokens, numbers)
  }
}
