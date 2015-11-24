package scorex.perma.actors

import java.security.SecureRandom

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.util.Timeout
import scorex.crypto.CryptographicHash._
import scorex.crypto.SigningFunctions.{PrivateKey, PublicKey, Signature}
import scorex.crypto._
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.crypto.ads.merkle.TreeStorage.Position
import scorex.perma.BlockchainBuilderSpec.WinningTicket
import scorex.perma.Storage.AuthDataStorage
import scorex.perma.actors.MinerSpec._
import scorex.perma.actors.TrustedDealerSpec.{SegmentsRequest, SegmentsToStore}
import scorex.perma.settings.{PermaSettings, Constants}
import scorex.perma.settings.Constants.DataSegment

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

case class PartialProof(signature: Signature, segmentIndex: Long, segment: AuthDataBlock[DataSegment])

case class Ticket(publicKey: PublicKey,
                  s: Array[Byte],
                  proofs: IndexedSeq[PartialProof])

class Miner(rootHash: Digest)(implicit settings: PermaSettings) extends Actor with ActorLogging {

  import Miner._

  private val keyPair = EllipticCurveImpl.createKeyPair(randomBytes(32))
  private implicit val timeout = Timeout(1 minute)

  private val segmentIds: Seq[Long] = 1.to(Constants.l).map { i =>
    u(keyPair._2, i - 1)
  }.toSeq

  //Mutable list of ids, remaining to download
  private var segmentToDownload: ListBuffer[Long] = segmentIds.to[ListBuffer]

  override def receive = {

    case Initialize(miners) =>
      log.debug("Initialize")

      val s = sender()

      segmentToDownload foreach { s =>
        if (authDataStorage.containsKey(s)) {
          segmentToDownload -= s
        }
      }

      if (segmentToDownload.nonEmpty) {
        miners.foreach(_ ! SegmentsRequest(segmentToDownload))
      }

    case GetStatus =>
      log.debug("Get status")
      if (segmentToDownload.isEmpty) {
        sender ! Initialized
      } else {
        sender ! LoadingData
      }

    case SegmentsRequest(ids) =>
      val segments: Subset = ids.map { x =>
        x -> authDataStorage.get(x)
      }.toMap.collect {
        case (key, Some(value)) => key -> value
      }
      log.info(s"Miner SegmentsRequest for ${ids.length} blocks returns ${segments.size} blocks")
      sender ! SegmentsToStore(segments)

    case SegmentsToStore(sgs) =>
      log.debug("SegmentsToStore({})", sgs)
      sgs.foreach { s =>
        if (segmentToDownload.contains(s._1) && s._2.check(s._1, rootHash)(Constants.hash)) {
          authDataStorage.set(s._1, s._2)
          segmentToDownload -= s._1
        }
      }
      authDataStorage.commit()

    case TicketGeneration(difficulty, puz) =>
      log.debug("TicketGeneration({})", puz)
      val ticket = generate(keyPair, puz)

      val check = validate(keyPair._2, puz, difficulty, ticket, rootHash)
      val score = ticketScore(ticket)
      log.debug("TicketGeneration result:{}, score:{}", check, score)

      if (check) {
        sender() ! WinningTicket(puz, score, ticket)
      } else {
        context.system.scheduler.scheduleOnce(200 millis, self, TicketGeneration(difficulty, puz))
      }

    case TicketValidation(difficulty, puz, t: Ticket) =>
      log.debug("TicketValidation({}, {})", puz, t)
      val res = validate(keyPair._2, puz, difficulty, t, rootHash)
      val score = ticketScore(t)
      log.debug("TicketValidation result:{}, score:{}", res, score)
  }
}

object Miner {

  val NoSig = Array[Byte]()

  //calculate index of i-th segment
  private def u(pubKey: PublicKey, i: Int): Long = {
    val h = Sha256.hash(pubKey ++ BigInt(i).toByteArray)
    BigInt(1, h).mod(Constants.n).toLong
  }


  //todo: move to utils
  def randomBytes(howMany: Int) = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides s
    r
  }

  def authDataStorage(implicit settings: PermaSettings) = new AuthDataStorage(settings.authDataStorage)

  def generate(keyPair: (PrivateKey, PublicKey), puz: Array[Byte])(implicit settings: PermaSettings): Ticket = {

    val (privateKey, publicKey) = keyPair

    //scratch-off for the Local-POR lottery
    val s = randomBytes(32)

    val sig0 = NoSig
    val r1 = u(publicKey, (BigInt(1, Sha256.hash(puz ++ publicKey ++ s)) % Constants.l).toInt)

    val proofs: IndexedSeq[PartialProof] = 1.to(Constants.k).foldLeft(
      (r1, sig0, Seq[PartialProof]())
    ) {
      case ((ri, sig_prev, seq), _) =>
        val segment = authDataStorage.get(ri).get
        val hi = Sha256.hash(puz ++ publicKey ++ sig_prev ++ segment.data)
        val sig = EllipticCurveImpl.sign(privateKey, hi)
        val r_next = u(publicKey, BigInt(1, Sha256.hash(puz ++ publicKey ++ sig)).mod(Constants.l).toInt)

        (r_next, sig, seq :+ PartialProof(sig, ri, segment))
    }._3.toIndexedSeq.ensuring(_.size == Constants.k)

    Ticket(publicKey, s, proofs)
  }

  //todo: validate r\i
  def validate(publicKey: PublicKey,
               puz: Array[Byte],
               difficulty: BigInt,
               t: Ticket,
               rootHash: CryptographicHash.Digest): Boolean = Try {
    val proofs = t.proofs
    require(proofs.size == Constants.k)

    //Local-POR lottery verification

    val sigs = NoSig +: proofs.map(_.signature)
    val ris = proofs.map(_.segmentIndex)

    val partialProofsCheck = 1.to(Constants.k).foldLeft(true) { case (partialResult, i) =>
      val segment = proofs(i - 1).segment

      segment.check(ris(i - 1), rootHash)() || {
        val hi = Sha256.hash(puz ++ publicKey ++ sigs(i - 1) ++ segment.data)
        EllipticCurveImpl.verify(sigs(i), hi, publicKey)
      }
    }
    partialProofsCheck && (ticketScore(t) < difficulty)
  }.getOrElse(false)

  def ticketScore(t: Ticket): BigInt = BigInt(1, Sha256.hash(t.proofs.map(_.signature).reduce(_ ++ _)))

}

//todo: split miner & fullnode roles: a fullnode makes validation, miner validation & generation

object MinerSpec {

  type Subset = Map[Position, AuthDataBlock[DataSegment]]

  case class Initialize(miners: Seq[ActorRef])

  case class TicketGeneration(difficulty: BigInt, puz: Array[Byte])

  case class TicketValidation(difficulty: BigInt, puz: Array[Byte], ticket: Ticket)

  case object GetStatus

  sealed trait MinerStatus

  case object Initialized extends MinerStatus

  case object LoadingData extends MinerStatus


}