package scorex.perma.network


import akka.actor.ActorRef
import scorex.app.Application
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.{SendToChosen, ViewSynchronizer}
import scorex.perma.settings.Constants._
import scorex.perma.settings.PermaSettings
import scorex.perma.storage.AuthDataStorage
import shapeless.Typeable._

/**
  *
  * Permacoin segments synchronization.
  * Some number of random peers are asked firstly.
  *
  * @param application - application
  */

class SegmentsSynchronizer(application: Application, rootHash: Array[Byte])(implicit settings: PermaSettings)
  extends ViewSynchronizer {

  override protected val networkControllerRef: ActorRef = application.networkController

  override val messageSpecs: Seq[MessageSpec[_]] = Seq()

  private val storage = new AuthDataStorage(settings.authDataStorage)

  override def receive: Receive = {
    case DataFromPeer(msgId, indexes: Seq[DataSegmentIndex]@unchecked, remote)
      if msgId == GetSegmentsMessageSpec.messageCode && indexes.cast[Seq[DataSegmentIndex]].isDefined =>
      ???
//
//      val segments:Seq[AuthDataBlock[DataSegment]] = indexes.flatMap(i => storage.get(i))
//      val msg = Message(SegmentsMessageSpec, Right(segments), None)
//      networkControllerRef ! SendToNetwork(msg, SendToChosen(Seq(remote)))
//
//    case DataFromPeer(msgId, segments:  Map[DataSegmentIndex, AuthDataBlock[DataSegment]]@unchecked, remote)
//      if msgId == SegmentsMessageSpec.messageCode && segments.cast[Map[DataSegmentIndex, AuthDataBlock[DataSegment]]].isDefined =>

//      segments.forall(s => s._2)
//      val segments = indexes.flatMap(i => storage.get(i))
//      val msg = Message(SegmentsMessageSpec, Right(segments), None)
//      networkControllerRef ! SendToNetwork(msg, SendToChosen(Seq(remote)))

    case _ => ???
  }
}
