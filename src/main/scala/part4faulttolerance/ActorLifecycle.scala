package part4faulttolerance

import akka.actor.{Actor, ActorLogging, ActorSystem, PoisonPill, Props}

object ActorLifecycle extends App {

  object StartChild

  class LifecycleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case StartChild => context.actorOf(Props[LifecycleActor], "child")
    }

    override def preStart(): Unit = log.info("I am starting")

    override def postStop(): Unit = log.info("I have stopped")
  }

  val system = ActorSystem("LifecycleDemo")
  val parent = system.actorOf(Props[LifecycleActor], "parent")
  parent ! StartChild
  parent ! PoisonPill

  /**
    * restart
    */
  object FailChild
  object Fail
  object CheckChild
  object Check

  class Parent extends Actor with ActorLogging {
    val child = context.actorOf(Props[Child], "supervisedChild")
    override def receive: Receive = {
      case FailChild => child ! Fail
      case CheckChild => child ! Check
    }
  }

  class Child extends Actor with ActorLogging {
    override def receive: Receive = {
      case Fail => log.warning("Child will fail now")
        throw new RuntimeException("I failed")
      case Check => log.info("alice and kicking")
    }

    override def preStart(): Unit = println("supervisedChild started")

    override def postStop(): Unit = println("supervisedChild stop")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.info(s"supervisedChild restarting because of ${reason.getMessage}")

    override def postRestart(reason: Throwable): Unit =
      log.info("supervisedChild restarted")
  }

  val supervisor = system.actorOf(Props[Parent], "supervisor")
  supervisor ! FailChild

  supervisor ! CheckChild

  // supervision strategy
}
