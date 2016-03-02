package scorex.perma.network


import akka.actor.ActorRef
import scorex.app.Application
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.{SendToChosen, ViewSynchronizer}
import scorex.perma.settings.PermaConstants._
import scorex.storage.Storage
import scorex.utils.ScorexLogging
import shapeless.Typeable._

import scala.util.Random

/**
  *
  * Permacoin segments synchronization.
  * Some number of random peers are asked firstly.
  *
  */
class SegmentsSynchronizer(application: Application, rootHash: Array[Byte], storage: Storage[Long, AuthDataBlock[DataSegment]])
  extends ViewSynchronizer with ScorexLogging {

  private val MaxSegmentsInMessage = 16

  override protected val networkControllerRef: ActorRef = application.networkController

  override val messageSpecs: Seq[MessageSpec[_]] = Seq(SegmentsMessageSpec, GetSegmentsMessageSpec)

  override def receive: Receive = {
    case DataFromPeer(msgId, indexes: Seq[DataSegmentIndex]@unchecked, remote)
      if msgId == GetSegmentsMessageSpec.messageCode && indexes.cast[Seq[DataSegmentIndex]].isDefined =>

      log.info("GetSegmentsMessage")

      val segments: Map[DataSegmentIndex, AuthDataBlock[DataSegment]] =
        Random.shuffle(indexes).take(MaxSegmentsInMessage).flatMap(i => storage.get(i).map(s => i -> s)).toMap

      val msg = Message(SegmentsMessageSpec, Right(segments), None)
      networkControllerRef ! SendToNetwork(msg, SendToChosen(Seq(remote)))

    case DataFromPeer(msgId, segments: Map[DataSegmentIndex, AuthDataBlock[DataSegment]]@unchecked, remote)
      if msgId == SegmentsMessageSpec.messageCode && segments.cast[Map[DataSegmentIndex, AuthDataBlock[DataSegment]]].isDefined =>
      log.info(s"SegmentsMessage with ${segments.size} segments")

      if (segments.forall(s => s._2.check(s._1, rootHash)(FastCryptographicHash))) {
        segments.foreach(s => storage.set(s._1, s._2))
        if(segments.nonEmpty) storage.commit()
      } else {
        log.warn(s"Incorrect segments from " + remote)
        //TODO blacklisting
      }

    case m => log.error(s"Unexpected message $m")
  }
}
