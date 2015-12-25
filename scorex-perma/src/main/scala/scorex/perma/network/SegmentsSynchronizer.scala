package scorex.perma.network


import akka.actor.ActorRef
import scorex.app.Application
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.{SendToChosen, ViewSynchronizer}
import scorex.perma.settings.Constants._
import scorex.perma.settings.{Constants, PermaSettings}
import scorex.perma.storage.AuthDataStorage
import scorex.utils.ScorexLogging
import shapeless.Typeable._

/**
  *
  * Permacoin segments synchronization.
  * Some number of random peers are asked firstly.
  *
  * @param application - application
  */

class SegmentsSynchronizer(application: Application, rootHash: Array[Byte])(implicit settings: PermaSettings)
  extends ViewSynchronizer with ScorexLogging {

  override protected val networkControllerRef: ActorRef = application.networkController

  override val messageSpecs: Seq[MessageSpec[_]] = Seq()

  private val storage = new AuthDataStorage(settings.authDataStorage)

  override def receive: Receive = {
    case DataFromPeer(msgId, indexes: Seq[DataSegmentIndex]@unchecked, remote)
      if msgId == GetSegmentsMessageSpec.messageCode && indexes.cast[Seq[DataSegmentIndex]].isDefined =>
      log.info("GetSegmentsMessage")
      val segments: Map[DataSegmentIndex, AuthDataBlock[DataSegment]] =
        indexes.map(i => i -> storage.get(i)).filter(_._2.isDefined).map(x => (x._1, x._2.get)).toMap
      val msg = Message(SegmentsMessageSpec, Right(segments), None)
      networkControllerRef ! SendToNetwork(msg, SendToChosen(Seq(remote)))

    case DataFromPeer(msgId, segments: Map[DataSegmentIndex, AuthDataBlock[DataSegment]]@unchecked, remote)
      if msgId == SegmentsMessageSpec.messageCode && segments.cast[Map[DataSegmentIndex, AuthDataBlock[DataSegment]]].isDefined =>
      log.info(s"SegmentsMessage with ${segments.size} segments")

      if (segments.forall(s => s._2.check(s._1, rootHash)(Constants.hash))) {
        segments.foreach(s => storage.set(s._1, s._2))
      } else {
        //TODO blacklisting
      }

    case m => log.error(s"Unexpected message $m")
  }
}