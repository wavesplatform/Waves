package scorex.perma

import akka.actor.{Actor, ActorRef}
import scorex.perma.actors.MinerSpec.{Initialize, Initialized, TicketGeneration}
import scorex.perma.actors.Ticket
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.util.Random


case class BlockHeaderLike(difficulty: BigInt, puz: Array[Byte], ticket: Ticket)

class BlockchainBuilder(miners: Seq[ActorRef]) extends Actor with ScorexLogging {

  import BlockchainBuilderSpec._

  var puz: Array[Byte] = calcPuz
  var initialized = 0

  val InitialDifficulty = BigInt(1, Array.fill(33)(1: Byte))
  val blockchainLike = mutable.Buffer[BlockHeaderLike]()


  private def calcPuz = 1.to(32).toArray.map(_ => Random.nextInt(256).toByte)

  def difficulty = blockchainLike.headOption.map(_.difficulty).getOrElse(InitialDifficulty)

  override def receive = {
    case Initialized =>
      initialized = initialized + 1
      if (initialized == miners.length) {
        log.info("start BlockchainBuilder")
        self ! SendWorkToMiners
      }

    case SendWorkToMiners =>
      miners.foreach { minerRef =>
        if (initialized == miners.length) {
          minerRef ! TicketGeneration(difficulty, puz)
        } else {
          minerRef ! Initialize
        }
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
        log.debug("Wrong puz from miner: " + minerPuz.mkString)
      }
  }
}

object BlockchainBuilderSpec {

  case object SendWorkToMiners

  case class WinningTicket(puz: Array[Byte], score: BigInt, t: Ticket)

}