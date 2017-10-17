package GuessTheNumber.parser.messages

private [GuessTheNumber] trait UserMessage
private [GuessTheNumber] case class IntegerFromUser(x: Integer) extends UserMessage
private [GuessTheNumber] case object WrongMessage extends UserMessage
