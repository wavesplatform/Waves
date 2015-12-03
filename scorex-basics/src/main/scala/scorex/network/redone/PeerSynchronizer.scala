package scorex.network.redone

import scorex.app.Application


class PeerSynchronizer(application:Application) extends ViewSynchronizer {

  import application.basicMessagesSpecsRepo._
  override val messageSpecs = Seq(GetPeersSpec, PeersSpec)

  override val networkControllerRef = application.networkController

  override def receive = {
    case _ =>
  }
}
