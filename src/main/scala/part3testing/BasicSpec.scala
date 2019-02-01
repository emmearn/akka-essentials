package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration._
import scala.util.Random

class BasicSpec extends TestKit(ActorSystem("BasicSpec")) with ImplicitSender with WordSpecLike with BeforeAndAfterAll {

  // setup
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

//  "The thing being tested" should {
//    "do this" in {
//      // testing scenario
//    }
//
//    "do another thing" in {
//      // testing scenario
//    }
//  }

    import BasicSpec._
    "A simple actor" should {
      "send back the same message" in {
        val echoActor = system.actorOf(Props[SimpleActor])
        val message = "hello, test"

        echoActor ! message
        expectMsg(message)
        testActor // made by "with ImplicitSender"
      }
    }

  "A blackhole actor" should {
    "send back some message" in {
      val blackhole = system.actorOf(Props[Blackhole])
      val message = "hello, test"

      blackhole ! message
      // expectMsg(message) // akka.test.single-expect-default
      expectNoMessage(1 second)
    }
  }

  "A lab test actor" should {
    val labTestActor = system.actorOf(Props[LabTestActor])

    "turn a string into uppercase" in {
      val message = "I love Akka"
      labTestActor ! message

      // expectMsg(message.toUpperCase)

      val reply = expectMsgType[String]

      assert(reply == message.toUpperCase)
      assert(reply.length == message.length)
    }

    "reply to a greeting" in {
      labTestActor ! "greeting"
      expectMsgAnyOf("hi", "hello")
    }

    "reply with favorite tech" in {
      labTestActor ! "favoriteTech"
      expectMsgAllOf("Scala", "Akka")
    }

    "reply with cool tech in a different way" in {
      labTestActor ! "favoriteTech"
      val messages = receiveN(2) // Seq[Any]

      // free to do more complicated assertions
    }

    "reply with cool tech in a fancy way" in {
      labTestActor ! "favoriteTech"

      expectMsgPF() {
        case "Scala" => // only care that the PF is defined
        case "Akka" =>
      }
    }
  }
}

object BasicSpec {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender ! message
    }
  }

  class Blackhole extends Actor {
    override def receive: Receive = Actor.emptyBehavior /*{
      case _ =>
    }*/
  }

  class LabTestActor extends Actor {
    val random = new Random()

    override def receive: Receive = {
      case "greeting" =>
        if(random.nextBoolean())
          sender ! "hi"
        else
          sender ! "hello"
      case "favoriteTech" =>
        sender ! "Scala"
        sender ! "Akka"
      case message: String => sender ! message.toUpperCase()
    }
  }
}