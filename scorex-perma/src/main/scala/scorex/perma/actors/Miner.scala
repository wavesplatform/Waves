package scorex.perma.actors

import java.security.SecureRandom

import akka.actor.{Actor, ActorRef}
import scorex.crypto.SigningFunctions.{Signature, PublicKey}
import scorex.crypto.{SigningFunctions, SigningFunctionsImpl}
import scorex.perma.Parameters
import scorex.perma.actors.MinerSpec._
import scorex.perma.actors.TrustedDealerSpec.SendOutSegments
import scorex.perma.merkle.AuthDataBlock
import scorex.perma.merkle.HashImpl.hash

case class Ticket(publicKey:PublicKey, s:Array[Byte], proof: Seq[(Signature, AuthDataBlock[Parameters.DataSegment])])

class Miner(trustedDealerRef: ActorRef) extends Actor {

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

      trustedDealerRef ! SendOutSegments(segmentIdsToDownload)

    case SegmentsToStore(sgs) =>
      require(segments.isEmpty)
      segments = sgs

    case TicketGeneration(puz) =>
      //scratch-off for the Local-POR lottery

      val s = randomBytes(32)

      val sig0 = Array[Byte]()
      val r1 = u(publicKey, BigInt(hash(puz ++ publicKey ++ s)).mod(Parameters.l).toInt)

      val proof = 1.to(Parameters.k).foldLeft(
        (r1, sig0, Seq[(SigningFunctions.Signature, AuthDataBlock[Parameters.DataSegment])]())
      ) {
        case ((ri, sig_prev, seq), _) =>

          val hi = hash(puz ++ publicKey ++ sig_prev ++ segments(ri).data)
          val sig = SigningFunctionsImpl.sign(privateKey, hi)

          val r_next = u(publicKey, BigInt(hash(puz ++ publicKey ++ sig)).mod(Parameters.l).toInt)

          (r_next, sig, seq :+ (sig, segments(ri)))
      }._3

      Ticket(publicKey, s, proof)




    case TicketValidation() =>
  }
}

//todo: split miner & fullnode roles: a fullnode makes validation, miner validation & generation

object MinerSpec {

  type Index = Int
  type Subset = Map[Index, AuthDataBlock[Parameters.DataSegment]]

  case class Initialize()

  case class SegmentsToStore(segments: Subset)

  case class TicketGeneration(puz: Array[Byte])

  case class TicketValidation()
}