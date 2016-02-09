package scorex.network

import akka.actor.{Actor, ActorRef}
import scorex.transaction.History._
import scorex.utils.ScorexLogging

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

//todo: break a connection if no score message from remote for some time?

class ScoreObserver(historySynchronizer: ActorRef) extends Actor with ScorexLogging {

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
    //todo: make configurable?
    val threshold = System.currentTimeMillis() - 1.minute.toMillis
    candidates.filter(_.seen > threshold)
  }

  override def preStart: Unit = {
    //todo: make configurable?
    context.system.scheduler.schedule(5.seconds, 5.seconds)(self ! UpdateScore(None))
  }

  override def receive: Receive = {
    case UpdateScore(scoreToAddOpt) =>
      val oldScore = candidates.headOption.map(_.score)
      candidates = clearOld(candidates)

      scoreToAddOpt.foreach { case (connectedPeer, value) =>
        candidates = candidates.filter(_.peer != connectedPeer)
        candidates = candidates :+ Candidate(connectedPeer, value, System.currentTimeMillis())
      }

      val ct = consider(candidates)
      candidates = ct._2

      val newScore = ct._1.map(_.score)
      val witnesses = candidates.map(_.peer)

      if (newScore.getOrElse(BigInt(0)) != oldScore.getOrElse(BigInt(0))) {
        historySynchronizer ! ConsideredValue(newScore, witnesses)
      }

    case GetScore =>
      candidates = clearOld(candidates)
      sender() ! ConsideredValue(candidates.headOption.map(_.score), candidates.map(_.peer))
  }
}

object ScoreObserver {
  case class UpdateScore(scoreToAdd: Option[(ConnectedPeer, BlockchainScore)])

  case object GetScore

  case class ConsideredValue(value: Option[BlockchainScore], witnesses: Seq[ConnectedPeer])
}