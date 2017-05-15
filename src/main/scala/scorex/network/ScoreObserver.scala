package scorex.network

import scorex.transaction.History._

object ScoreObserver {

  case class UpdateScore(peer: ConnectedPeer, score: BlockchainScore)

  case object GetScore

  case class CurrentScore(scores: Seq[(ConnectedPeer, BlockchainScore)])

  private case class ScoreData(score: BlockchainScore, seen: Long)

  private type Candidates = Map[ConnectedPeer, ScoreData]
}
