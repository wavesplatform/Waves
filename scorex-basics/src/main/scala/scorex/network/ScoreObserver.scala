package scorex.network

import scorex.app.Application
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.MessageSpec
import scorex.transaction.History
import scorex.transaction.History._
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

//todo: break a connection if no score message from remote for some time?

class ScoreObserver(application: Application) extends ViewSynchronizer with ScorexLogging {

  import ScoreObserver._
  import application.basicMessagesSpecsRepo._

  override val messageSpecs: Seq[MessageSpec[_]] = Seq(ScoreMessageSpec)

  override val networkControllerRef = application.networkController
  private val coordinator = application.coordinator

  private val scoreTTL = application.settings.scoreTTL

  private case class PeerData(score: BlockchainScore, seen: Long)
  private type Candidates = Map[ConnectedPeer, PeerData]

  private var candidates: Candidates = Map.empty

  context.system.scheduler.schedule(5.seconds, 5.seconds, self, UpdateScore(None))

  override def receive: Receive = {
    //todo: check sender
    case DataFromPeer(msgId, score: History.BlockchainScore, connectedPeer) if msgId == ScoreMessageSpec.messageCode =>
      self ! UpdateScore(Some(connectedPeer -> score))

    case UpdateScore(scoreToAdd) =>
      val oldMaxScore = maxScore(candidates)
      candidates = clearOld(candidates)

      scoreToAdd.foreach { case (connectedPeer, value) =>
        candidates = candidates + (connectedPeer -> PeerData(value, System.currentTimeMillis()))
      }

      val newMaxScore = maxScore(candidates)

      if (newMaxScore > oldMaxScore) {
        coordinator ! consideredValue
      }

    case GetScore =>
      candidates = clearOld(candidates)
      sender() ! consideredValue

    //the signal to initialize
    case Unit =>
  }

  private def clearOld(candidates: Candidates): Candidates = {
    val threshold = System.currentTimeMillis() - scoreTTL.toMillis
    candidates.filter(_._2.seen > threshold)
  }

  private def maxScore(candidates: Candidates): BlockchainScore =
    if (candidates.isEmpty) 0 else candidates.values.map(_.score).max

  private def consideredValue: ConsideredValue = {
    ConsideredValue(candidates.map { case (peer, data) => (peer, data.score) } toSeq)
  }
}

object ScoreObserver {

  case class UpdateScore(scoreToAdd: Option[(ConnectedPeer, BlockchainScore)])

  case object GetScore

  case class ConsideredValue(scores: Seq[(ConnectedPeer, BlockchainScore)])

}