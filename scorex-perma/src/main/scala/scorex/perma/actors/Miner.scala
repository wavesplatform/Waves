package scorex.perma.actors

import java.security.SecureRandom

import akka.actor.{Actor, ActorLogging, ActorRef}
import scorex.crypto.CryptographicHash._
import scorex.crypto.SigningFunctions.{PrivateKey, PublicKey, Signature}
import scorex.crypto.ads.merkle.{MerkleTree, AuthDataBlock}
import scorex.crypto.{CryptographicHash, Sha256, SigningFunctions, SigningFunctionsImpl}
import scorex.perma.BlockchainBuilderSpec.WinningTicket
import scorex.perma.Parameters
import scorex.perma.actors.MinerSpec._
import scorex.perma.actors.TrustedDealerSpec.{SegmentsRequest, SegmentsToStore}


import scala.util.Try

case class PartialProof(signature: Signature, segmentIndex: Int, segment: AuthDataBlock[Parameters.DataSegment])

case class Ticket(publicKey: PublicKey,
                  s: Array[Byte],
                  proofs: IndexedSeq[PartialProof])

class Miner(trustedDealerRef: ActorRef, rootHash: Digest) extends Actor with ActorLogging {

  import Miner._

  private val keyPair = SigningFunctionsImpl.createKeyPair(randomBytes(32))

  private var segments: Subset = Map()

  override def receive = {

    case Initialize =>
      log.info("Initialize")

      val segmentIdsToDownload = 1.to(Parameters.l).map { i =>
        u(keyPair._2, i - 1)
      }.toArray

      trustedDealerRef ! SegmentsRequest(segmentIdsToDownload)

    case SegmentsToStore(sgs) =>
      log.debug("SegmentsToStore({})", sgs)
      require(segments.isEmpty)
      segments = sgs

    case TicketGeneration(difficulty, puz) =>
      log.debug("TicketGeneration({})", puz)
      val ticket = generate(keyPair, puz, segments)

      val check = validate(keyPair._2, puz, difficulty, ticket, rootHash)
      val score = ticketScore(ticket)
      log.debug("TicketGeneration result:{}, score:{}", check, score)

      if (check) {
        sender() ! WinningTicket(puz, score, ticket)
      } else {
        Thread.sleep(100)
        self ! TicketGeneration(difficulty, puz)
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
  private def u(pubKey: SigningFunctions.PublicKey, i: Int): Int = {
    val h = Sha256.hash(pubKey ++ BigInt(i).toByteArray)
    BigInt(1, h).mod(Parameters.n).toInt
  }


  //todo: move to utils
  def randomBytes(howMany: Int) = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides s
    r
  }

  def generate(keyPair: (PrivateKey, PublicKey), puz: Array[Byte], segments: Subset): Ticket = {

    val (privateKey, publicKey) = keyPair

    //scratch-off for the Local-POR lottery
    val s = randomBytes(32)

    val sig0 = NoSig
    val r1 = u(publicKey, (BigInt(1, Sha256.hash(puz ++ publicKey ++ s)) % Parameters.l).toInt)
      .ensuring(r => segments.keySet.contains(r))

    val proofs = 1.to(Parameters.k).foldLeft(
      (r1, sig0, Seq[PartialProof]())
    ) {
      case ((ri, sig_prev, seq), _) =>
        val hi = Sha256.hash(puz ++ publicKey ++ sig_prev ++ segments(ri).data)
        val sig = SigningFunctionsImpl.sign(privateKey, hi)
        val r_next = u(publicKey, BigInt(1, Sha256.hash(puz ++ publicKey ++ sig)).mod(Parameters.l).toInt)
          .ensuring(r => segments.keySet.contains(r))

        (r_next, sig, seq :+ PartialProof(sig, ri, segments(ri)))
    }._3.toIndexedSeq.ensuring(_.size == Parameters.k)

    Ticket(publicKey, s, proofs)
  }

  //todo: validate r\i
  def validate(publicKey: PublicKey,
               puz: Array[Byte],
               difficulty: BigInt,
               t: Ticket,
               rootHash: CryptographicHash.Digest): Boolean = Try {
    val proofs = t.proofs
    require(proofs.size == Parameters.k)

    //Local-POR lottery verification

    val sigs = NoSig +: proofs.map(_.signature)
    val ris = proofs.map(_.segmentIndex)

    val partialProofsCheck = 1.to(Parameters.k).foldLeft(true) { case (partialResult, i) =>
      val segment = proofs(i - 1).segment

      segment.check(ris(i - 1), rootHash)() || {
        val hi = Sha256.hash(puz ++ publicKey ++ sigs(i - 1) ++ segment.data)
        SigningFunctionsImpl.verify(sigs(i), hi, publicKey)
      }
    }
    partialProofsCheck && (ticketScore(t) < difficulty)
  }.getOrElse(false)

  def ticketScore(t: Ticket): BigInt = BigInt(1, Sha256.hash(t.proofs.map(_.signature).reduce(_ ++ _)))

}

//todo: split miner & fullnode roles: a fullnode makes validation, miner validation & generation

object MinerSpec {

  type Index = Int
  type Subset = Map[Index, AuthDataBlock[Parameters.DataSegment]]

  case class Initialize()

  case class TicketGeneration(difficulty: BigInt, puz: Array[Byte])

  case class TicketValidation(difficulty: BigInt, puz: Array[Byte], ticket: Ticket)

}