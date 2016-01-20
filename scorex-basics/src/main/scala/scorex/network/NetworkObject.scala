package scorex.network

import akka.actor.ActorRef
import scorex.network.NetworkObject.ConsideredValue
import scorex.transaction.History._

case object UpdateNetworkView

//not thread-safe!!!
trait NetworkObject[V] {

  val localComponentRef: ActorRef

  def consider(candidates: Map[ConnectedPeer, V]): (Option[V], Seq[ConnectedPeer], Map[ConnectedPeer, V])

  private var candidates = Map[ConnectedPeer, V]()

  private var _consideredValue: Option[ConsideredValue[V]] = None

  def consideredValue: Option[ConsideredValue[V]] = _consideredValue

  def betterThan(newValue: Option[V], oldValue: Option[V]): Boolean

  def networkUpdate(remote: ConnectedPeer, value: V): Unit = {
    candidates += remote -> value
    val ct = consider(candidates)
    candidates = ct._3

    val cValue = ct._1
    val witnesses = ct._2

    if (betterThan(cValue, _consideredValue.flatMap(_.value))) {
      //todo: cache considered value and send signal only if that > previous
      val cv = ConsideredValue(cValue, witnesses)
      _consideredValue = Some(cv)
      localComponentRef ! cv
    }
  }
}

object NetworkObject {
  case class ConsideredValue[V](value: Option[V], witnesses: Seq[ConnectedPeer])
}


class ScoreNetworkObject(override val localComponentRef: ActorRef) extends NetworkObject[BlockchainScore] {

  override def consider(candidates: Map[ConnectedPeer, BlockchainScore])
  : (Option[BlockchainScore], Seq[ConnectedPeer], Map[ConnectedPeer, BlockchainScore]) = {
    val bestNetworkScore = candidates.maxBy(_._2)._2
    val witnesses = candidates.filter(_._2 == bestNetworkScore).keys.toSeq
    (Some(bestNetworkScore), witnesses, candidates.filter(_._2 == bestNetworkScore))
  }

  override def betterThan(newValue: Option[BlockchainScore],
                          oldValue: Option[BlockchainScore]): Boolean =
    newValue.getOrElse(BigInt(0)) > oldValue.getOrElse(BigInt(0))
}
