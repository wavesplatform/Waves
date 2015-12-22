package scorex.perma.network

import scorex.network.message.Message.MessageCode
import scorex.network.message.MessageSpec
import scorex.perma.settings.Constants.{DataSegment, DataSegmentIndex}

import scala.util.Try

class SegmentsMessageSpec extends MessageSpec[Map[DataSegmentIndex, DataSegment]] {
  override val messageCode: MessageCode = 51: Byte

  override def serializeData(data: Map[DataSegmentIndex, DataSegment]): Array[Byte] = ???

  override def deserializeData(bytes: Array[Byte]): Try[Map[DataSegmentIndex, DataSegment]] = ???

  override val messageName: String = _
}

class GetSegmentsMessageSpec extends MessageSpec[Seq[DataSegmentIndex]] {
  override val messageCode: MessageCode = 52: Byte

  override val messageName: String = _

  override def serializeData(data: Seq[DataSegmentIndex]): Array[MessageCode] = ???

  override def deserializeData(bytes: Array[MessageCode]): Try[Seq[DataSegmentIndex]] = ???
}
