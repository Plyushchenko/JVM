package GuessTheNumber.parser

import GuessTheNumber.parser.messages._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

private class MessageParser extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r
  val integerFromUser: Parser[UserMessage] = "^0|([1-9][0-9]{0,6})$".r ^^ {
    x => IntegerFromUser(x.toInt)
  }
  val userMessage: Parser[UserMessage] = integerFromUser
}

private [GuessTheNumber] object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}