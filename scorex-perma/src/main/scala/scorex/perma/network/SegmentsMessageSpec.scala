package scorex.perma.network

import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.network.message.Message.MessageCode
import scorex.network.message.MessageSpec
import scorex.perma.settings.Constants.{DataSegment, DataSegmentIndex}

import scala.util.Try

class SegmentsMessageSpec extends MessageSpec[Map[DataSegmentIndex, DataSegment]] {
  override val messageCode: MessageCode = 51: Byte

  override def serializeData(data: Map[DataSegmentIndex, DataSegment]): Array[Byte] = ???

  override def deserializeData(bytes: Array[Byte]): Try[Map[DataSegmentIndex, DataSegment]] = ???

  override val messageName: String = "SegmentsMessage"
}

class GetSegmentsMessageSpec extends MessageSpec[Seq[DataSegmentIndex]] {
  override val messageCode: MessageCode = 52: Byte

  override val messageName: String = "GetSegmentsMessage"

  private val DataLength = 8

  override def serializeData(idexes: Seq[DataSegmentIndex]): Array[Byte] = {
    val length = idexes.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), 4, 0)

    idexes.foldLeft(lengthBytes) { case (bs, index) =>
      Bytes.concat(bs, Bytes.ensureCapacity(Longs.toByteArray(index), DataLength, 0))
    }
  }

  override def deserializeData(bytes: Array[MessageCode]): Try[Seq[DataSegmentIndex]] = Try {
    val length = Ints.fromByteArray(bytes.slice(0, 4))
    require(bytes.length == 4 + (length * DataLength))
    (0 until length).map { i =>
      val position = 4 + i * DataLength
      Longs.fromByteArray(bytes.slice(position, position + DataLength))
    }
  }
}
