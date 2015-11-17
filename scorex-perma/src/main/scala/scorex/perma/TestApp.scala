package scorex.perma

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.slf4j.LoggerFactory
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.BlockchainBuilderSpec.{SendWorkToMiners, WinningTicket}
import scorex.perma.actors.MinerSpec.{Initialize, TicketGeneration}
import scorex.perma.actors.{Miner, Ticket, TrustedDealer}

import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.util.Random



object TestApp extends App {

  val MinersCount = 10

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

  miners.foreach(minerRef => minerRef ! Initialize)

  Thread.sleep(2000)

  log.info("start BlockchainBuilder")

  val blockchainBuilder = actorSystem.actorOf(Props(classOf[BlockchainBuilder], miners))
  blockchainBuilder ! BlockchainBuilderSpec.SendWorkToMiners
}
