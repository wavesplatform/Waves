package scorex.perma.network


import akka.actor.ActorRef
import scorex.app.Application
import scorex.network.ViewSynchronizer
import scorex.network.message.MessageSpec

/**
  *
  * Permacoin segments synchronization.
  * Some number of random peers are asked firstly.
  *
  * @param application - application
  */

class SegmentsSynchronizer(application: Application) extends ViewSynchronizer {

  override protected val networkControllerRef: ActorRef = application.networkController

  override val messageSpecs: Seq[MessageSpec[_]] = Seq()

  override def receive: Receive = {
    case _ => ???
  }
}
