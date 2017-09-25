package GuessTheNumber.bot

import GuessTheNumber.database.GuessTheNumberActor._
import GuessTheNumber.parser.MessageParser
import GuessTheNumber.parser.messages._
import akka.actor.ActorRef
import akka.util.Timeout
import akka.pattern.ask
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.{Random, Success}

class GuessTheNumberBot(val token: String, val database: ActorRef)
    extends TelegramBot with Polling with Commands {
  private val gameSessions: mutable.HashMap[Long, Int] = mutable.HashMap.empty
  private val random = new Random()
  private val upperBound = 1000 + 1
  private val commands = Array("/help", "/start", "/newGame", "/giveUp", "/stats")
  private val help =
    """/newGame to start a new game
      |/giveUp to give up
      |/stats to show stats
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
        println(gameSessions(command.chat.id))
        reply("Guess the number in [0; 1000]!")
      }
    }
  }

  onCommand('giveUp) {
    implicit command =>
      gameSessions.get(command.chat.id) match {
        case Some(_) =>
          gameSessions -= command.chat.id
          database ! AddDefeat(command.chat.id)
          reply("You gave up!")
        case None => reply("You're not playing!")
      }
  }

  onCommand('stats) {
    implicit command => {
      implicit val timeout: Timeout = Timeout(1.second)
      (database ? GetStats(command.chat.id)).onComplete {
        case Success(Stats((x, y))) => reply("+ " + x + "\n- " + y + "\n% " + 100d * x / (x + y))
        case _ => reply("Database error.")
      }
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
                     database ! AddVictory(message.chat.id)
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