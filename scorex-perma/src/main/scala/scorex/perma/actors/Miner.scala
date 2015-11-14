package scorex.perma.actors

import java.security.SecureRandom

import akka.actor.{ActorRef, Actor}
import scorex.perma.Parameters
import scorex.perma.actors.MinerSpec.Initialize
import scorex.perma.actors.TrustedDealerSpec.SendOutSegments
import scorex.perma.merkle.HashImpl
import scorex.crypto.SigningFunctionsImpl

class Miner(trustedDealerRef: ActorRef) extends Actor {

  override def receive = {

    case Initialize =>

      val seed = new Array[Byte](32)
      new SecureRandom().nextBytes(seed) //overrides seed
      val (privKey, pubKey) = SigningFunctionsImpl.createKeyPair(seed)

      val segmentIdsToDownload = 1.to(Parameters.l).map{j =>
        val hash = HashImpl.hash(pubKey ++ BigInt(j).toByteArray)
        BigInt(hash.toArray).mod(Parameters.n).toInt
      }.toArray

      trustedDealerRef ! SendOutSegments(segmentIdsToDownload)
  }
}

object MinerSpec{
  case class Initialize()
}
