package GuessTheNumber.bot

import GuessTheNumber.parser.MessageParser
import GuessTheNumber.parser.messages._
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import scala.collection.mutable
import scala.util.Random

class GuessTheNumberBot(val token: String) extends TelegramBot with Polling with Commands {
  private val gameSessions: mutable.HashMap[Long, Int] = mutable.HashMap.empty
  private val random = new Random()
  private val upperBound = 1E6.toInt + 1
  private val commands = Array("/help", "/start", "/newGame", "/giveUp")
  private val help =
    """/newGame to start a new game
      |/giveUp to give up
      |/help to show this
    """.stripMargin

  onCommand('start, 'help) {
    implicit message => reply(help)
  }

  onCommand('newGame) {
    implicit command => {
      if (gameSessions.contains(command.chat.id)) {
        reply("You're playing!")
      } else {
        gameSessions(command.chat.id) = random.nextInt(upperBound)
        reply("Guess the number [0; 1'000'000]!")
      }
    }
  }

  onCommand('giveUp) {
    implicit command =>
      gameSessions.get(command.chat.id) match {
        case Some(_) =>
          gameSessions -= command.chat.id
          reply("You gave up!")
        case None => reply("You're not playing!")
      }
  }

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        if (!commands.contains(text)) {
          MessageParser.parse(text) match {
            case IntegerFromUser(x) =>
               gameSessions.get(message.chat.id) match {
                 case Some(y) =>
                   if (x == y) {
                     gameSessions -= message.chat.id
                     reply("Correct!")
                   } else {
                     reply("My number is " + (if (y > x) "bigger" else "less") +  " than yours!")
                   }
                 case None => reply("You're not playing!")
               }
            case WrongMessage =>
              reply("Unknown message, sorry.")
          }
        }
      }
  }

}