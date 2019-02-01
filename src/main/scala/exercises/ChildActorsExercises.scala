package exercises

import akka.actor.{Actor, ActorPath, ActorRef, ActorSystem, Props}

object ChildActorsExercises extends App {

  // Distributed Word counting
  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class WordCountTask(text: String)
    case class WordCountReply(workersPath: ActorPath, text: String, count: Int)

  }
  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(nChildren) =>
        val workers = for (i <- 1 to nChildren) yield context.actorOf(Props[WordCounterWorker], s"worker_$i")
        context.become(initialized(workers))
    }
//    override def receive: Receive = {
//      case Initialize(nChildren) =>
//        context.become(initialized(initializeWorkers(nChildren, List[ActorRef]())))
//    }
//
//    def initializeWorkers(nChildren: Int, workers: List[ActorRef]): List[ActorRef] = {
//      if(nChildren == 0)
//        workers
//      else
//        initializeWorkers(nChildren - 1, workers :+ system.actorOf(Props[WordCounterWorker], s"worker$nChildren"))
//    }

    def initialized(workers: Seq[ActorRef]): Receive = {
      case WordCountReply(path, text, count) => println(s"My workers $path have counts $count words in text '${text}'")
      case text: String =>
        workers.head ! WordCountTask(text)
        context.become(initialized(workers.tail :+ workers.head))
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(text) => sender ! WordCountReply(self.path, text, text.split(" ").length)
    }
  }

  import WordCounterMaster.Initialize
  val system = ActorSystem("ChildActorsExercises")
  val wordCounterMaster = system.actorOf(Props[WordCounterMaster])
  wordCounterMaster ! Initialize(3)

  wordCounterMaster ! "Akka is awesome"
  wordCounterMaster ! "test for counts"
  wordCounterMaster ! "another test for counts"
  wordCounterMaster ! "this should be restart with first worker"
  wordCounterMaster ! "is actually restarted ?"
}
