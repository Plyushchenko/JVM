package GuessTheNumber

import GuessTheNumber.bot.GuessTheNumberBot
import akka.actor.{ActorSystem, Props}
import database.GuessTheNumberActor

object Main extends App {
  val token = "418103142:AAFdT9xFwmhLyLZnaiiQkF1_6bmu38zXkcg"
  val system = ActorSystem()
  val database = system.actorOf(Props(classOf[GuessTheNumberActor]))
  private val bot = new GuessTheNumberBot(token, database)
  bot.run()
}
