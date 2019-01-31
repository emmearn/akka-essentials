package exercises

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorBehavior extends App {

  object Counter {
    // DOMAIN of the counter
    case object Increment
    case object Decrement
    case object Print

    def props = Props(new Counter)
  }

  class Counter extends Actor {
    import Counter._
    var count = 0

    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Increment =>
        println(s"[countReceive $currentCount] incrementing")
        context.become(countReceive(currentCount + 1))
      case Decrement =>
        println(s"[countReceive $currentCount] decrementing")
        context.become(countReceive(currentCount - 1))
      case Print => println(s"[countReceive($currentCount)] my current count is $currentCount")
    }
  }

  val system = ActorSystem("myActorSystem")
  val counter = system.actorOf(Counter.props, "myCounter")

  import Counter._

  counter ! Print
  (1 to 3).foreach(_ => counter ! Increment)
  counter ! Print
  counter ! Decrement
  counter ! Print



  case class Vote(candidate: String)
  case object VoteStatusRequest
  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor {
    // var candidate: Option[String] = None

    override def receive: Receive = {
      case Vote(c) => context.become(voted(c)) // candidate = Some(c)
      case VoteStatusRequest => sender ! VoteStatusReply(None) // sender ! VoteStatusReply(candidate)
    }

    def voted(candidate: String): Receive = {
      case VoteStatusRequest => sender ! VoteStatusReply(Some(candidate))
    }
  }

  case class AggregateVotes(citizens: Set[ActorRef])
  class VoteAggregator extends Actor {
    // var stillWaiting: Set[ActorRef] = Set()
    // var currentStats: Map[String, Int] = Map()

    override def receive: Receive = {
      awaitingCommand
//      case AggregateVotes(citizens) =>
//        stillWaiting = citizens
//        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
//      case VoteStatusReply(None) =>
//        // a citizen hasn't voted yet
//        sender ! VoteStatusRequest // this might end up in an infinite loop
//      case VoteStatusReply(Some(candidate)) =>
//        val newStillWaiting = stillWaiting - sender
//        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
//        currentStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
//
//        if(newStillWaiting.isEmpty)
//          println(s"[aggregator] pool stats: $currentStats")
//        else
//          stillWaiting = newStillWaiting
    }

    def awaitingCommand: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
        context.become(awaitingStatuses(citizens, Map()))
    }

    def awaitingStatuses(stillWaiting: Set[ActorRef], currentStats: Map[String, Int]): Receive = {
      case VoteStatusReply(None) =>
        // a citizen hasn't voted yet
        sender ! VoteStatusRequest // this might end up in an infinite loop
      case VoteStatusReply(Some(candidate)) =>
        val newStillWaiting = stillWaiting - sender
        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
        val newStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))

        if(newStillWaiting.isEmpty)
          println(s"[aggregator] pool stats: $newStats") // println(s"[aggregator] pool stats: $currentStats")
        else // still need to process some statuses
          context.become(awaitingStatuses(newStillWaiting, newStats))
    }
  }

  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val daniel = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))
}
