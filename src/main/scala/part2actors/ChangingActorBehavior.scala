package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChangingActorBehavior extends App {

  object FussyKid {
    case object KidAccept
    case object KidReject
    val HAPPY = "happy"
    val SAD = "sad"

    def props = Props(new FussyKid)
    def statelessProps = Props(new StatelessFussyKid)
  }
  class FussyKid extends Actor {
    import FussyKid._
    import Mom._

    // internal state of the kid
    var state = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) =>
        if(state == HAPPY)
          sender ! KidAccept
        else
          sender ! KidReject
    }
  }
//  class StatelessFussyKid extends Actor {
//    import FussyKid._
//    import Mom._
//    override def receive: Receive = happyReceive
//
//    def happyReceive: Receive = {
//      case Food(VEGETABLE) => context.become(sadReceive) // change my receive handler to sadReceive
//      case Food(CHOCOLATE) => // stay happy
//      case Ask(_) => sender ! KidAccept
//    }
//
//    def sadReceive: Receive = {
//      case Food(VEGETABLE) => // stay sad
//      case Food(CHOCOLATE) => context.become(happyReceive) // change my receive handler to happyReceive
//      case Ask(_) => sender ! KidReject
//    }
//  }
  class StatelessFussyKid extends Actor {
    import FussyKid._
    import Mom._
    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false)
      case Food(CHOCOLATE) =>
      case Ask(_) => sender ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false)
      case Food(CHOCOLATE) => context.unbecome()
      case Ask(_) => sender ! KidReject
    }
  }

  object Mom {
    case class MomStart(kidRef: ActorRef)
    case class Food(food: String)
    case class Ask(message: String) // do you want to play?
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"

    def props = Props(new Mom)
  }
  class Mom extends Actor {
    import Mom._
    import FussyKid._

    override def receive: Receive = {
      case MomStart(kidRef) =>
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Ask("do you want to play?")
      case KidAccept => println("Yay, my kid is happy!")
      case KidReject => println("My kid is sad, but as he's healthy!")
    }
  }

  val system = ActorSystem("changingActorBehaviorDemo")
  val fussyKid = system.actorOf(FussyKid.props)
  val statelessFussyKid = system.actorOf(FussyKid.statelessProps)
  val mom = system.actorOf(Mom.props)

  import Mom.MomStart
  mom ! MomStart(fussyKid)
  mom ! MomStart(statelessFussyKid)

  /*
    mom receives MomStart
      kid receives Food(veg) -> kid will change the handler to sadReceive
      kid receives Ask(play?) -> kid replies with the sadReceive handler =>
    mom receives KidReject
   */

  /*
  context.become
    Food(veg) -> stack.push(sadReceive)
    Food(chocolate) -> stack.push(happyReceive)
    Stack:
    1. happyReceive
    2. sadReceive
    3. happyReceive
   */

  /*
    new behavior
    Food(veg)
    Food(veg)
    Food(choco)
    Food(choco)
    Stack:
    1. happyReceive
   */
}
