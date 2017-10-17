package GuessTheNumber.database

import akka.persistence.PersistentActor

import scala.collection.mutable

private [GuessTheNumber] class GuessTheNumberActor extends PersistentActor {
  import GuessTheNumberActor._
  val map: mutable.HashMap[Long, (Int, Int)] = mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
      case AddVictory(id) =>
        map.get(id) match {
          case Some((won, lost)) => map(id) = (won + 1, lost)
          case None => map(id) = (1, 0)
        }
      case AddDefeat(id) =>
        map.get(id) match {
          case Some((won, lost)) => map(id) = (won, lost + 1)
          case None => map(id) = (0, 1)
        }
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetStats(id) => sender ! Stats(map.getOrElse(id, (0, 0)))
  }

  override def persistenceId = "guess-the-number-database"
}

object GuessTheNumberActor {
  trait Event
  case class AddVictory(id: Long) extends Event
  case class AddDefeat(id: Long) extends Event
  case class GetStats(id: Long)
  case class Stats(stats: (Int, Int))
}
