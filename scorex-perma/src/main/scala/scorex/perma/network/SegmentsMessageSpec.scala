package scorex.perma.network

import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.network.message.Message.MessageCode
import scorex.network.message.MessageSpec
import scorex.perma.settings.PermaConstants.{DataSegment, DataSegmentIndex}

import scala.util.Try


object SegmentsMessageSpec extends MessageSpec[Map[DataSegmentIndex, AuthDataBlock[DataSegment]]] {
  override val messageCode: MessageCode = 51: Byte

  override def serializeData(data: Map[DataSegmentIndex, AuthDataBlock[DataSegment]]): Array[Byte] = {
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(data.size), 4, 0)
    if (data.nonEmpty) {
      val authDataBlockSize = AuthDataBlock.encode(data.head._2).length
      val authDataBlockSizeBytes = Bytes.ensureCapacity(Ints.toByteArray(authDataBlockSize), 4, 0)

      data.foldLeft(lengthBytes ++ authDataBlockSizeBytes) { case (bs, dataSegment) =>
        bs ++ Bytes.ensureCapacity(Longs.toByteArray(dataSegment._1), 8, 0) ++
          Bytes.ensureCapacity(AuthDataBlock.encode(dataSegment._2), authDataBlockSize, 0)
      }
    } else {
      lengthBytes
    }
  }

  override def deserializeData(bytes: Array[Byte]): Try[Map[DataSegmentIndex, AuthDataBlock[DataSegment]]] = Try {
    val length = Ints.fromByteArray(bytes.slice(0, 4))
    if (length > 0) {
      val authDataBlockSize = Ints.fromByteArray(bytes.slice(4, 8))
      require(bytes.length == 8 + length * (8 + authDataBlockSize))
      (0 until length).map { i =>
        val position = 8 + i * (8 + authDataBlockSize)
        val index = Longs.fromByteArray(bytes.slice(position, position + 8))
        index -> AuthDataBlock.decode(bytes.slice(position + 8, position + 8 + authDataBlockSize)).get
      }.toMap
    } else {
      Map.empty
    }
  }

  override val messageName: String = "SegmentsMessage"
}

object GetSegmentsMessageSpec extends MessageSpec[Seq[DataSegmentIndex]] {
  override val messageCode: MessageCode = 52: Byte

  override val messageName: String = "GetSegmentsMessage"

  private val SegmentIndexLength = 8

  override def serializeData(indexes: Seq[DataSegmentIndex]): Array[Byte] = {
    val length = indexes.length
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), 4, 0)

    indexes.foldLeft(lengthBytes) { case (bs, index) =>
      Bytes.concat(bs, Bytes.ensureCapacity(Longs.toByteArray(index), SegmentIndexLength, 0))
    }
  }

  override def deserializeData(bytes: Array[MessageCode]): Try[Seq[DataSegmentIndex]] = Try {
    val length = Ints.fromByteArray(bytes.slice(0, 4))
    require(bytes.length == 4 + length * SegmentIndexLength)
    (0 until length).map { i =>
      val position = 4 + i * SegmentIndexLength
      Longs.fromByteArray(bytes.slice(position, position + SegmentIndexLength))
    }
  }
}

object PermacoinMessagesRepo {
  val specs = Seq(GetSegmentsMessageSpec, SegmentsMessageSpec)
}
