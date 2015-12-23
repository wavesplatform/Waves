package scorex.perma.network

import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.network.message.Message.MessageCode
import scorex.network.message.MessageSpec
import scorex.perma.settings.Constants
import scorex.perma.settings.Constants.{DataSegment, DataSegmentIndex}

import scala.util.Try

class SegmentsMessageSpec extends MessageSpec[Map[DataSegmentIndex, DataSegment]] {
  override val messageCode: MessageCode = 51: Byte
  private lazy val SegmentSize = Constants.segmentSize

  override def serializeData(data: Map[DataSegmentIndex, DataSegment]): Array[Byte] = {
    val length = data.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), 4, 0)

    data.foldLeft(lengthBytes) { case (bs, dataSegment) =>
      Bytes.concat(bs, Bytes.ensureCapacity(Longs.toByteArray(dataSegment._1), 8, 0),
        Bytes.ensureCapacity(dataSegment._2, SegmentSize, 0))
    }
  }

  override def deserializeData(bytes: Array[Byte]): Try[Map[DataSegmentIndex, DataSegment]] = Try {
    val length = Ints.fromByteArray(bytes.slice(0, 4))
    require(bytes.length == 4 + length * (8 + SegmentSize))
    (0 until length).map { i =>
      val position = 4 + i * (8 + SegmentSize)
      val index = Longs.fromByteArray(bytes.slice(position, position + 8))
      val dataSegment = bytes.slice(position + 8, position + 8 + SegmentSize)
      index -> dataSegment
    }.toMap
  }

  override val messageName: String = "SegmentsMessage"
}

class GetSegmentsMessageSpec extends MessageSpec[Seq[DataSegmentIndex]] {
  override val messageCode: MessageCode = 52: Byte

  override val messageName: String = "GetSegmentsMessage"

  private val DataLength = 8

  override def serializeData(idexes: Seq[DataSegmentIndex]): Array[Byte] = {
    val length = idexes.length
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), 4, 0)

    idexes.foldLeft(lengthBytes) { case (bs, index) =>
      Bytes.concat(bs, Bytes.ensureCapacity(Longs.toByteArray(index), DataLength, 0))
    }
  }

  override def deserializeData(bytes: Array[MessageCode]): Try[Seq[DataSegmentIndex]] = Try {
    val length = Ints.fromByteArray(bytes.slice(0, 4))
    require(bytes.length == 4 + length * DataLength)
    (0 until length).map { i =>
      val position = 4 + i * DataLength
      Longs.fromByteArray(bytes.slice(position, position + DataLength))
    }
  }
}
