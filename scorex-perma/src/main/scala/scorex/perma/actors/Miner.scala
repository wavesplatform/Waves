package scorex.perma.actors

import java.security.SecureRandom

import akka.actor.{ActorLogging, Actor, ActorRef}
import scorex.crypto.SigningFunctions.{PrivateKey, PublicKey, Signature}
import scorex.crypto.{SigningFunctions, SigningFunctionsImpl}
import scorex.perma.Parameters
import scorex.perma.actors.MinerSpec._
import scorex.perma.actors.TrustedDealerSpec.{SegmentsRequest, SegmentsToStore}
import scorex.perma.merkle.CryptographicHash.Digest
import scorex.perma.merkle.HashImpl.hash
import scorex.perma.merkle.{AuthDataBlock, CryptographicHash, MerkleTree}

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
        u(keyPair._2, i)
      }.toArray

      trustedDealerRef ! SegmentsRequest(segmentIdsToDownload)

    case SegmentsToStore(sgs) =>
      log.info("SegmentsToStore({})", sgs)
      require(segments.isEmpty)
      segments = sgs

    case TicketGeneration(puz) =>
      log.info("TicketGeneration({})", puz)
      val ticket = generate(keyPair, puz, segments)
    //todo: check ticket


    case TicketValidation(puz, t: Ticket) =>
      log.info("TicketValidation({}, {})", puz, t)
      val res = validate(keyPair._2, puz, t, rootHash)

  }
}

object Miner {

  val NoSig = Array[Byte]()

  //calculate index of i-th segment
  private def u(pubKey: SigningFunctions.PublicKey, i: Int): Int = {
    val h = hash(pubKey ++ BigInt(i).toByteArray)
    BigInt(h).mod(Parameters.n).toInt
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
    val r1 = u(publicKey, BigInt(hash(puz ++ publicKey ++ s)).mod(Parameters.l).toInt)

    val proofs = 1.to(Parameters.k).foldLeft(
      (r1, sig0, Seq[PartialProof]())
    ) {
      case ((ri, sig_prev, seq), _) =>
        val hi = hash(puz ++ publicKey ++ sig_prev ++ segments(ri).data)
        val sig = SigningFunctionsImpl.sign(privateKey, hi)
        val r_next = u(publicKey, BigInt(hash(puz ++ publicKey ++ sig)).mod(Parameters.l).toInt)
        (r_next, sig, seq :+ PartialProof(sig, ri, segments(ri)))
    }._3.toIndexedSeq.ensuring(_.size == Parameters.k)

    Ticket(publicKey, s, proofs)
  }

  //todo: validate ri
  def validate(publicKey: PublicKey, puz: Array[Byte], t: Ticket, rootHash: CryptographicHash.Digest): Boolean = Try {
    val proofs = t.proofs
    require(proofs.size == Parameters.k)

    //Local-POR lottery verification

    val sigs = NoSig +: proofs.map(_.signature)
    val ris = proofs.map(_.segmentIndex)

    1.to(Parameters.k).foldLeft(true) { case (partialResult, i) =>
      val segment = proofs(i - 1).segment

      MerkleTree.check(ris(i - 1), rootHash, segment.data, segment.merklePath)() || {
        val hi = hash(puz ++ publicKey ++ sigs(i - 1) ++ segment.data)
        SigningFunctionsImpl.verify(sigs(i), hi, publicKey)
      }
    }
  }.getOrElse(false)
}

//todo: split miner & fullnode roles: a fullnode makes validation, miner validation & generation

object MinerSpec {

  type Index = Int
  type Subset = Map[Index, AuthDataBlock[Parameters.DataSegment]]

  case class Initialize()

  case class TicketGeneration(puz: Array[Byte])

  case class TicketValidation(puz: Array[Byte], ticket: Ticket)

}