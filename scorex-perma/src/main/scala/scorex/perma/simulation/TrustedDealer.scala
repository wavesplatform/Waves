package scorex.perma.simulation

import akka.actor.{Actor, ActorLogging}
import scorex.crypto.CryptographicHash
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.simulation.MinerSpec.Subset
import scorex.perma.simulation.TrustedDealerSpec.{SegmentsRequest, SegmentsToStore}
import scorex.perma.settings.Constants


class TrustedDealer[H <: CryptographicHash](val tree: MerkleTree[H]) extends Actor with ActorLogging {

  override def receive = {
    case SegmentsRequest(segmentIds) =>
      log.info(s"Dealer SegmentsRequest for ${segmentIds.length} blocks")

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

  case class SegmentsRequest(segments: Seq[Long])

  case class SegmentsToStore(segments: Subset)

}
