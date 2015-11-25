package scorex.perma

import akka.actor.{Actor, ActorRef}
import scorex.crypto.{Sha256, CryptographicHash}
import scorex.perma.actors.MinerSpec._
import scorex.perma.actors.Ticket
import scorex.utils._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random


case class BlockHeaderLike(difficulty: BigInt, puz: Array[Byte], ticket: Ticket)

class BlockchainBuilder(miners: Seq[ActorRef], dealer: ActorRef) extends Actor with ScorexLogging {

  import BlockchainBuilderSpec._

  var initialized = 0

  val InitialDifficulty = BigInt(1, Array.fill(33)(1: Byte))
  val blockchainLike = mutable.Buffer[BlockHeaderLike]()

  def difficulty = blockchainLike.headOption.map(_.difficulty).getOrElse(InitialDifficulty)

  def calcPuz(block: Option[BlockHeaderLike], hash: CryptographicHash = Sha256): Array[Byte] = block match {
    case Some(b) =>
      hash.hash(b.puz ++ b.ticket.s ++ b.ticket.publicKey
        ++ b.ticket.proofs.foldLeft(Array.empty: Array[Byte])((b, a) => b ++ a.signature))
    case None =>
      hash.hash("Scorex perma genesis")
  }

  override def receive = {
    case s: MinerStatus =>
      s match {
        case Initialized =>
          initialized = initialized + 1
          if (initialized == miners.length) {
            log.info("All miners initialized")
            self ! SendWorkToMiners
          }
        case LoadingData =>
          sender() ! Initialize(Seq(dealer))
          context.system.scheduler.scheduleOnce(200 millis, sender(), GetStatus)
      }

    case SendWorkToMiners =>
      miners.foreach { minerRef =>
        if (initialized == miners.length) {
          minerRef ! TicketGeneration(difficulty, calcPuz(blockchainLike.lastOption))
        } else {
          minerRef ! Initialize(miners)
          context.system.scheduler.scheduleOnce(200 millis, minerRef, GetStatus)
        }
      }

    //miners are honest (lol) in our setting, so no validation here
    case WinningTicket(minerPuz, score, ticket) =>
      val puz = calcPuz(blockchainLike.lastOption)
      if (minerPuz sameElements puz) {
        val newBlock = BlockHeaderLike(score, puz, ticket)
        log.info(s"Block generated: $newBlock, blockchain size: ${blockchainLike.size}")
        blockchainLike += newBlock
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