package scorex.perma.actors

import akka.actor.{Actor, ActorLogging}
import scorex.crypto.CryptographicHash
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.Parameters
import scorex.perma.actors.MinerSpec.Subset
import scorex.perma.actors.TrustedDealerSpec.{SegmentsRequest, SegmentsToStore}


class TrustedDealer[H <: CryptographicHash](val tree: MerkleTree[H]) extends Actor with ActorLogging {

  override def receive = {
    case SegmentsRequest(segmentIds) =>
      log.info(s"SegmentsRequest(${segmentIds.mkString(", ")})")

      assert(segmentIds.length == Parameters.l)

      val segments: Subset = segmentIds.map { x =>
        x -> tree.byIndex(x)
      }.toMap.collect {
        case (key, Some(value)) => key -> value
      }
      sender ! SegmentsToStore(segments)
    case m =>
      log.warning("Unknown message: {}", m)
  }

}

object TrustedDealerSpec {

  case object PublishDataset

  case class SegmentsRequest(segments: Array[Int])

  case class SegmentsToStore(segments: Subset)

}
