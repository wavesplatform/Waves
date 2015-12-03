package scorex.network.redone

import akka.actor.ActorRef
import scorex.network.message.BasicMessagesRepo.{PeersSpec, GetPeersSpec}


class PeerSynchronizer(override val networkControllerRef: ActorRef) extends ViewSynchronizer {

  override val messageSpecs = Seq(GetPeersSpec, PeersSpec)

  override def receive = {
    case _ =>
  }
}
