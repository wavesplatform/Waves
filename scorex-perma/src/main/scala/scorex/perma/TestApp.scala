package scorex.perma

import akka.actor.{ActorRef, Props, ActorSystem}
import org.slf4j.LoggerFactory
import scorex.perma.actors.MinerSpec.Initialize
import scorex.perma.actors.{Miner, TrustedDealer}
import scorex.perma.merkle.{HashImpl, MerkleTree}

import scala.util.Random

object TestApp extends App {

  val log = LoggerFactory.getLogger(this.getClass)

  import Parameters._

  log.info("Generating random data set")
  val rnd = new Random()
  val dataSet = (1 to segmentSize).map(x => Random.alphanumeric.take(n).mkString).toArray.map(_.getBytes)

  log.info("Calculate tree")
  val tree = MerkleTree.create(dataSet)

  log.info("start actor system")
  protected lazy val actorSystem = ActorSystem("lagonaki")
  val dealer = actorSystem.actorOf(Props(classOf[TrustedDealer], dataSet))
  val miners: Seq[ActorRef] = (1 to 10).map(x => actorSystem.actorOf(Props(classOf[Miner], dealer, tree.hash)))

  log.info("start sending requests")
  miners.head ! Initialize
//  miners.map { m =>
//    m ! Initialize
//  }


}
