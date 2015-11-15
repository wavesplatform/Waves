package scorex.perma

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.slf4j.LoggerFactory
import scorex.perma.BlockchainBuilderSpec.{SendWorkToMiners, WinningTicket}
import scorex.perma.actors.MinerSpec.{Initialize, TicketGeneration}
import scorex.perma.actors.{Miner, Ticket, TrustedDealer}
import scorex.perma.merkle.MerkleTree
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.util.Random


case class BlockHeaderLike(difficulty: BigInt, puz: Array[Byte], ticket: Ticket)

class BlockchainBuilder(miners: Seq[ActorRef]) extends Actor with ScorexLogging {

  var puz: Array[Byte] = calcPuz

  val InitialDifficulty = BigInt(1, Array.fill(33)(1: Byte))
  val blockchainLike = mutable.Buffer[BlockHeaderLike]()


  private def calcPuz = 1.to(10).toArray.map(_ => Random.nextInt(256).toByte)

  def difficulty = blockchainLike.headOption.map(_.difficulty).getOrElse(InitialDifficulty)

  override def receive = {
    case SendWorkToMiners =>
      miners.foreach { minerRef =>
        minerRef ! TicketGeneration(difficulty, puz)
      }

    //miners are honest (lol) in our setting, so no validation here
    case WinningTicket(minerPuz, score, ticket) =>
      if (minerPuz sameElements puz) {
        val newBlock = BlockHeaderLike(score, puz, ticket)
        log.info(s"Block generated: $newBlock, blockchain size: ${blockchainLike.size}")
        blockchainLike += newBlock
        puz = calcPuz
        self ! SendWorkToMiners
      } else {
        sender() ! TicketGeneration(difficulty, puz)
        log.info("Wrong puz from miner: " + minerPuz.mkString)
      }
  }
}

object BlockchainBuilderSpec {

  case object SendWorkToMiners

  case class WinningTicket(puz: Array[Byte], score: BigInt, t: Ticket)

}

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
