package scorex.perma

import akka.actor.{ActorRef, Props, ActorSystem}
import org.slf4j.LoggerFactory
import scorex.perma.actors.MinerSpec.{TicketGeneration, Initialize}
import scorex.perma.actors.{Miner, TrustedDealer}
import scorex.perma.merkle.MerkleTree

import scala.util.Random

object TestApp extends App {

  val MinersCount = 1

  val log = LoggerFactory.getLogger(this.getClass)

  import Parameters._

  log.info("Generating random data set")
  val rnd = new Random()
  val dataSet = (1 to n).map(x => Random.alphanumeric.take(segmentSize).mkString).toArray.map(_.getBytes)

  log.info("Calculate tree")
  val tree = MerkleTree.create(dataSet)

  log.info("start actor system")
  protected lazy val actorSystem = ActorSystem("lagonaki")
  val dealer = actorSystem.actorOf(Props(classOf[TrustedDealer], dataSet))
  val miners: Seq[ActorRef] = (1 to MinersCount).map(x => actorSystem.actorOf(Props(classOf[Miner], dealer, tree.hash)))

  log.info("start sending requests")
//  miners.head ! Initialize
  miners.foreach { m =>
    m ! Initialize
    Thread.sleep(1000)
  }

  miners.head ! TicketGeneration(Array(1:Byte))
}
