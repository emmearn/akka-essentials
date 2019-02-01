package exercises

import akka.actor.{Actor,  ActorRef, ActorSystem, Props}

object ChildActorsExercises extends App {

  // Distributed Word counting
  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class WordCountTask(id: Int, text: String)
    case class WordCountReply(id: Int, count: Int)

  }
  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(nChildren) =>
        println("[master] initializing...")
        val workers = for (i <- 1 to nChildren) yield context.actorOf(Props[WordCounterWorker], s"worker_$i")
        context.become(initialized(workers, 1, Map()))
    }

    def initialized(workers: Seq[ActorRef], taskId: Int, requestMap: Map[Int, ActorRef]): Receive = {
      case WordCountReply(id, count) =>
        requestMap(id) ! count
        println(s"[master] I have received a reply for task id $id with $count")
        context.become(initialized(workers, taskId, requestMap - id))
      case text: String =>
        println(s"[master] I have received: $text - I will send it to child $taskId")
        workers.head ! WordCountTask(taskId, text)
        context.become(initialized(workers.tail :+ workers.head, taskId + 1, requestMap + (taskId -> sender)))
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(id, text) =>
        println(s"${self.path} I have received task $id with $text")
        sender ! WordCountReply(id, text.split(" ").length)
    }
  }

  class TestActor extends Actor {
    override def receive: Receive = {
      case "go" =>
        import WordCounterMaster.Initialize
        val master = context.actorOf(Props[WordCounterMaster], "master")
        master ! Initialize(3)

        val texts = List("I love akka", "Scala is super dope", "yes", "mee to")

        texts.foreach(text => master ! text)
      case count: Int =>
        println(s"[test actor] I received a reply $count")
    }
  }

  val system = ActorSystem("roundRobinWordCountExercise")
  val testActor = system.actorOf(Props[TestActor], "testActir")
  testActor ! "go"
}
