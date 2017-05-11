package scorex.network

import akka.actor.ActorRef
import com.wavesplatform.settings.SynchronizationSettings
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.MessageSpec
import scorex.transaction.History
import scorex.transaction.History._
import scorex.utils.ScorexLogging
import scorex.network.message._

import scala.language.postfixOps

class ScoreObserver(protected val networkControllerRef: ActorRef, coordinator: ActorRef, synchronizationSettings: SynchronizationSettings)
  extends ViewSynchronizer with ScorexLogging {

  import ScoreObserver._

  override val messageSpecs: Seq[MessageSpec[_]] = Seq(ScoreMessageSpec)

  private val scoreTTL = synchronizationSettings.scoreTTL

  private var candidates: Candidates = Map.empty

  override def receive: Receive = {
    case DataFromPeer(msgId, score: History.BlockchainScore, connectedPeer) if msgId == ScoreMessageSpec.messageCode =>
      self ! UpdateScore(connectedPeer, score)

    case UpdateScore(peer, updatedScore) =>
      val oldMaxScore = maxScore(candidates)
      candidates = clearOld(candidates)

      candidates += peer -> ScoreData(updatedScore, System.currentTimeMillis())

      val newMaxScore = maxScore(candidates)

      if (newMaxScore > oldMaxScore) {
        coordinator ! currentScore
      }

    case GetScore =>
      candidates = clearOld(candidates)
      sender() ! currentScore
  }

  private def clearOld(candidates: Candidates): Candidates = {
    val threshold = System.currentTimeMillis() - scoreTTL.toMillis
    candidates.filter(_._2.seen > threshold)
  }

  private def maxScore(candidates: Candidates): BlockchainScore =
    if (candidates.isEmpty) 0 else candidates.values.map(_.score).max

  private def currentScore: CurrentScore = {
    CurrentScore(candidates.map { case (peer, data) => (peer, data.score) } toSeq)
  }
}

object ScoreObserver {

  case class UpdateScore(peer: ConnectedPeer, score: BlockchainScore)

  case object GetScore

  case class CurrentScore(scores: Seq[(ConnectedPeer, BlockchainScore)])

  private case class ScoreData(score: BlockchainScore, seen: Long)

  private type Candidates = Map[ConnectedPeer, ScoreData]
}
