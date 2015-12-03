package scorex.network.redone

import akka.actor.ActorRef
import scorex.network.ConnectedPeer
import scorex.network.message.MessageSpec
import scorex.network.redone.NetworkObject.ConsideredValue
import scorex.transaction.History
import scorex.transaction.History._

/**
  * Created by kushti on 03.12.15.
  */
object NetworkObject {

  case class ConsideredValue[V](value: V, witnesses: Seq[ConnectedPeer])

  // case class LocalUpdate[V](localUpdate: Option[V])
  //case class NetworkUpdate[V](remote: ConnectedPeer, value: V)
}


case class ListenForNetworkMessages(specs: Seq[MessageSpec[_]], listener: ActorRef)

case object UpdateNetworkView

//todo: typed actor?
//not thread-safe!!!
trait NetworkObject[V] {

  //val networkControllerRef: ActorRef
  val localComponentRef: ActorRef

  //mutable
  private var candidates = Map[ConnectedPeer, V]()

  //private var localValue: Option[V] = None

  def consider(candidates: Map[ConnectedPeer, V]): (Option[V], Seq[ConnectedPeer], Map[ConnectedPeer, V])

  def networkUpdate(remote: ConnectedPeer, value: V) = {
    candidates += remote -> value
    val ct = consider(candidates)
    candidates = ct._3

    val consideredValue = ct._1
    val witnesses = ct._2

    //todo: cache considered value and send signal only if that > previous
    localComponentRef ! ConsideredValue(consideredValue, witnesses)
  }

  /*
  override def receive = {
    case LocalUpdate(localUpdate: Option[V]) =>
      localValue = localUpdate
      localUpdate.foreach { lv =>
        networkControllerRef ! (messageSpec, sendingStrategy)
      }

    case NetworkUpdate(remote: ConnectedPeer, value: V) =>
      candidates += remote -> value
      val ct = consider(candidates)
      consideredValue = ct._1
      candidates = ct._2
      localComponentRef ! value
  } */
}



class ScoreNetworkObject(//override val networkControllerRef: ActorRef,
                         override val localComponentRef: ActorRef) extends NetworkObject[History.BlockchainScore] {

  override def consider(candidates: Map[ConnectedPeer, BlockchainScore])
  : (Option[BlockchainScore], Map[ConnectedPeer, BlockchainScore]) = {
    val bestNetworkScore = candidates.maxBy(_._2)._2
    (Some(bestNetworkScore), candidates.filter(_._2 == bestNetworkScore))
  }

}
