package scorex.perma.actors

import java.security.SecureRandom

import akka.actor.{Actor, ActorRef}
import scorex.crypto.SigningFunctions.{PublicKey, Signature}
import scorex.crypto.{SigningFunctions, SigningFunctionsImpl}
import scorex.perma.Parameters
import scorex.perma.actors.MinerSpec._
import scorex.perma.actors.TrustedDealerSpec.{SegmentsRequest, SegmentsToStore}
import scorex.perma.merkle.HashImpl.hash
import scorex.perma.merkle.{AuthDataBlock, CryptographicHash}

import scala.util.Try

case class PartialProof(signature: Signature, segmentIndex: Int, segment: AuthDataBlock[Parameters.DataSegment])

case class Ticket(publicKey: PublicKey,
                  s: Array[Byte],
                  proofs: IndexedSeq[PartialProof])

class Miner(trustedDealerRef: ActorRef, rootHash: CryptographicHash.Digest) extends Actor {

  val NoSig = Array[Byte]()

  private val (privateKey, publicKey) = SigningFunctionsImpl.createKeyPair(randomBytes(32))

  private var segments: Subset = Map()

  private def randomBytes(howMany: Int) = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides s
    r
  }

  //calculate index of i-th segment
  private def u(pubKey: SigningFunctions.PublicKey, i: Int): Int = {
    val h = hash(pubKey ++ BigInt(i).toByteArray)
    BigInt(h).mod(Parameters.n).toInt
  }

  override def receive = {

    case Initialize =>

      val segmentIdsToDownload = 1.to(Parameters.l).map { i =>
        u(publicKey, i)
      }.toArray

      trustedDealerRef ! SegmentsRequest(segmentIdsToDownload)

    case SegmentsToStore(sgs) =>
      require(segments.isEmpty)
      segments = sgs

    case TicketGeneration(puz) =>
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

    case TicketValidation(puz, Ticket(pk, s, proofs)) =>

      require(proofs.size == Parameters.k)
      Try {

        //Local-POR lottery verification

        val sigs = NoSig +: proofs.map(_.signature)
        val ris = proofs.map(_.segmentIndex)


        /*
        1.to(Parameters.k).foldLeft(true){case (partialResult, i) =>
            //val
        }

        val r1 = u(publicKey, BigInt(hash(puz ++ publicKey ++ s)).mod(Parameters.l).toInt)

        val result = proofs.foldLeft(((sig0, r1), true)) { case (((sig, ri), res), pi) =>
          val hi = hash(puz ++ publicKey ++ sig_prev ++ segments(ri).data)
        } */


      }
  }
}

//todo: split miner & fullnode roles: a fullnode makes validation, miner validation & generation

object MinerSpec {

  type Index = Int
  type Subset = Map[Index, AuthDataBlock[Parameters.DataSegment]]

  case class Initialize()


  case class TicketGeneration(puz: Array[Byte])

  case class TicketValidation(puz: Array[Byte], ticket: Ticket)

}