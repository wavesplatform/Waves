package scorex.network

import akka.actor.{Actor, ActorRef}
import scorex.transaction.History._

import scala.concurrent.duration._

//todo: break a connection if no score message from remote for some time?

class ScoreObserver(historySynchronizer: ActorRef) extends Actor {

  import ScoreObserver._

  private case class Candidate(peer: ConnectedPeer, score: BlockchainScore, seen: Long)

  private var candidates = Seq[Candidate]()

  private def consider(candidates: Seq[Candidate]): (Option[Candidate], Seq[Candidate]) =
    candidates.isEmpty match {
      case true => (None, Seq())
      case false =>
        val bestNetworkScore = candidates.maxBy(_.score).score
        val witnesses = candidates.filter(_.score == bestNetworkScore)
        (witnesses.headOption, witnesses)
    }

  private def clearOld(candidates: Seq[Candidate]): Seq[Candidate] = {
    val threshold = System.currentTimeMillis() - 1.minute.toMillis
    candidates.filter(_.seen > threshold)
  }


  override def receive: Receive = {
    case PutScore(connectedPeer: ConnectedPeer, value: BlockchainScore) =>
      candidates = clearOld(candidates)
      val curScore = candidates.headOption.map(_.score)

      candidates = candidates.filter(_.peer != connectedPeer)

      candidates = candidates :+ Candidate(connectedPeer, value, System.currentTimeMillis())
      val ct = consider(candidates)
      candidates = ct._2

      val cValue = ct._1.map(_.score)
      val witnesses = candidates.map(_.peer)

      if (cValue.getOrElse(BigInt(0)) > curScore.getOrElse(BigInt(0))) {
        historySynchronizer ! ConsideredValue(cValue, witnesses)
      }

    case GetScore =>
      candidates = clearOld(candidates)
      sender() ! ConsideredValue(candidates.headOption.map(_.score), candidates.map(_.peer))
  }
}

object ScoreObserver {
  case class PutScore(connectedPeer: ConnectedPeer, value: BlockchainScore)

  case object GetScore

  case class ConsideredValue(value: Option[BlockchainScore], witnesses: Seq[ConnectedPeer])
}