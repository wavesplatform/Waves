package scorex.network.message

import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.network.{GetBlock, GetSignatures, KnownPeers, Signatures, GetPeers}
import com.wavesplatform.state2.ByteStr
import scorex.block.Block
import scorex.crypto.signatures.SigningFunctions
import scorex.crypto.signatures.SigningFunctions.Signature
import scorex.network.message.Message._
import scorex.network.{BlockCheckpoint, Checkpoint}
import scorex.transaction.History
import scorex.transaction.TransactionParser._

import scala.util.Try


object GetPeersSpec extends MessageSpec[GetPeers.type] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def deserializeData(bytes: Array[Byte]): Try[GetPeers.type] =
    Try {
      require(bytes.isEmpty, "Non-empty data for GetPeers")
      GetPeers
    }

  override def serializeData(data: GetPeers.type): Array[Byte] = Array()
}

object PeersSpec extends MessageSpec[KnownPeers] {
  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val messageName: String = "Peers message"

  override def deserializeData(bytes: Array[Byte]): Try[KnownPeers] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * (AddressLength + PortLength)), "Data does not match length")

    KnownPeers((0 until length).map { i =>
      val position = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
      val address = InetAddress.getByAddress(addressBytes)
      val portBytes = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    })
  }

  override def serializeData(peers: KnownPeers): Array[Byte] = {
    val length = peers.peers.size
    val lengthBytes = Ints.toByteArray(length)

    peers.peers.foldLeft(lengthBytes) { case (bs, peer) =>
      Bytes.concat(bs, peer.getAddress.getAddress, Ints.toByteArray(peer.getPort))
    }
  }
}

trait SignaturesSeqSpec[A <: AnyRef] extends MessageSpec[A] {

  import scorex.transaction.TransactionParser.SignatureLength

  private val DataLength = 4

  def wrap(signatures: Seq[SigningFunctions.Signature]): A
  def unwrap(v: A): Seq[SigningFunctions.Signature]

  override def deserializeData(bytes: Array[Byte]): Try[A] = Try {
    val lengthBytes = bytes.take(DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * SignatureLength), "Data does not match length")

    wrap((0 until length).map { i =>
      val position = DataLength + (i * SignatureLength)
      bytes.slice(position, position + SignatureLength)
    })
  }

  override def serializeData(v: A): Array[Byte] = {
    val signatures = unwrap(v)
    val length = signatures.size
    val lengthBytes = Ints.toByteArray(length)

    //WRITE SIGNATURES
    signatures.foldLeft(lengthBytes) { case (bs, header) => Bytes.concat(bs, header) }
  }
}

object GetSignaturesSpec extends SignaturesSeqSpec[GetSignatures] {
  override def wrap(signatures: Seq[Signature]) = GetSignatures(signatures.map(ByteStr(_)))
  override def unwrap(v: GetSignatures) = v.signatures.map(_.bytes)

  override val messageCode: MessageCode = 20: Byte
  override val messageName: String = "GetSignatures message"
}

object SignaturesSpec extends SignaturesSeqSpec[Signatures] {
  override def wrap(signatures: Seq[Signature]) = Signatures(signatures.map(ByteStr(_)))
  override def unwrap(v: Signatures) = v.signatures.map(_.bytes)

  override val messageCode: MessageCode = 21: Byte
  override val messageName: String = "Signatures message"
}

object GetBlockSpec extends MessageSpec[GetBlock] {
  override val messageCode: MessageCode = 22: Byte
  override val messageName: String = "GetBlock message"

  override def serializeData(signature: GetBlock): Array[Byte] = signature.signature.bytes

  override def deserializeData(bytes: Array[Byte]): Try[GetBlock] = Try {
    require(bytes.length == scorex.transaction.TransactionParser.SignatureLength, "Data does not match length")
    GetBlock(ByteStr(bytes))
  }
}

object BlockMessageSpec extends MessageSpec[Block] {
  override val messageCode: MessageCode = 23: Byte

  override val messageName: String = "Block message"

  override def serializeData(block: Block): Array[Byte] = block.bytes

  override def deserializeData(bytes: Array[Byte]): Try[Block] = Block.parseBytes(bytes)
}

object ScoreMessageSpec extends MessageSpec[History.BlockchainScore] {
  override val messageCode: MessageCode = 24: Byte

  override val messageName: String = "Score message"

  override def serializeData(score: History.BlockchainScore): Array[Byte] = {
    val scoreBytes = score.toByteArray
    val bb = java.nio.ByteBuffer.allocate(scoreBytes.length)
    bb.put(scoreBytes)
    bb.array()
  }

  override def deserializeData(bytes: Array[Byte]): Try[History.BlockchainScore] = Try {
    BigInt(1, bytes)
  }
}

object CheckpointMessageSpec extends MessageSpec[Checkpoint] {
  override val messageCode: MessageCode = 100: Byte

  override val messageName: String = "Checkpoint message"

  override def serializeData(checkpoint: Checkpoint): Array[Byte] =
    Bytes.concat(checkpoint.toSign, checkpoint.signature)

  override def deserializeData(bytes: Array[Byte]): Try[Checkpoint] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, Ints.BYTES)
    val length = Ints.fromByteArray(lengthBytes)

    require(length <= Checkpoint.MaxCheckpoints)

    val HeightLength = Ints.BYTES

    val items = (0 until length).map { i =>
      val position = lengthBytes.length + (i * (HeightLength + SignatureLength))
      val heightBytes = util.Arrays.copyOfRange(bytes, position, position + HeightLength)
      val height = Ints.fromByteArray(heightBytes)
      val blockSignature = util.Arrays.copyOfRange(bytes, position + HeightLength, position + HeightLength + SignatureLength)
      BlockCheckpoint(height, blockSignature)
    }

    val signature = bytes.takeRight(SignatureLength)

    Checkpoint(items, signature)
  }
}


object BasicMessagesRepo {
  val specs: Seq[MessageSpec[_ <: AnyRef]] = Seq(GetPeersSpec, PeersSpec, GetSignaturesSpec, SignaturesSpec,
    GetBlockSpec, BlockMessageSpec, ScoreMessageSpec, CheckpointMessageSpec)
}