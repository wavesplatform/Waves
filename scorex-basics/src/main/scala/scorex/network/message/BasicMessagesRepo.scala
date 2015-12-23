package scorex.network.message

import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.crypto.SigningFunctions._
import scorex.crypto.{EllipticCurveImpl, SigningFunctions}
import scorex.network.message.Message._
import scorex.transaction.{History, TransactionModule}

import scala.util.Try


class BasicMessagesRepo()(implicit val transactionalModule: TransactionModule[_],
                          consensusModule: ConsensusModule[_]) {

  object GetPeersSpec extends MessageSpec[Unit] {
    override val messageCode: Message.MessageCode = 1: Byte

    override val messageName: String = "GetPeers message"

    override def deserializeData(bytes: Array[Byte]): Try[Unit] =
      Try(require(bytes.isEmpty, "Non-empty data for GetPeers"))

    override def serializeData(data: Unit) = Array()
  }

  object PeersSpec extends MessageSpec[Seq[InetSocketAddress]] {
    private val AddressLength = 4
    private val PortLength = 4
    private val DataLength = 4

    override val messageCode: Message.MessageCode = 2: Byte

    override val messageName: String = "Peers message"

    override def deserializeData(bytes: Array[Byte]): Try[Seq[InetSocketAddress]] = Try {
      //READ LENGTH
      val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
      val length = Ints.fromByteArray(lengthBytes)

      //CHECK IF DATA MATCHES LENGTH
      if (bytes.length != DataLength + (length * (AddressLength + PortLength)))
        throw new Exception("Data does not match length")

      (0 until length).map { i =>
        val position = lengthBytes.length + (i * (AddressLength + PortLength))
        val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
        val address = InetAddress.getByAddress(addressBytes)
        val portBytes = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
        new InetSocketAddress(address, Ints.fromByteArray(portBytes))
      }
    }

    override def serializeData(peers: Seq[InetSocketAddress]) = {
      val length = peers.size
      val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DataLength, 0)

      peers.foldLeft(lengthBytes) { case (bs, peer) =>
        Bytes.concat(bs,
          peer.getAddress.getAddress, Bytes.ensureCapacity(Ints.toByteArray(peer.getPort), 4, 0))
      }
    }
  }

  trait SignaturesSeqSpec extends MessageSpec[Seq[SigningFunctions.Signature]] {

    import scorex.crypto.EllipticCurveImpl.SignatureLength

    private val DataLength = 4

    override def deserializeData(bytes: Array[Byte]): Try[Seq[Signature]] = Try {
      val lengthBytes = bytes.take(DataLength)
      val length = Ints.fromByteArray(lengthBytes)

      //CHECK IF DATA MATCHES LENGTH
      if (bytes.length != DataLength + (length * SignatureLength))
        throw new Exception("Data does not match length")

      //CREATE HEADERS LIST
      (0 to length - 1).map { i =>
        val position = DataLength + (i * SignatureLength)
        bytes.slice(position, position + SignatureLength)
      }.toSeq
    }

    override def serializeData(signatures: Seq[Signature]): Array[Byte] = {
      val length = signatures.size
      val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DataLength, 0)

      //WRITE SIGNATURES
      signatures.foldLeft(lengthBytes) { case (bs, header) => Bytes.concat(bs, header) }
    }
  }

  object GetSignaturesSpec extends SignaturesSeqSpec {
    override val messageCode: MessageCode = 20: Byte
    override val messageName: String = "GetSignatures message"
  }

  object SignaturesSpec extends SignaturesSeqSpec {
    override val messageCode: MessageCode = 21: Byte
    override val messageName: String = "Signatures message"
  }

  object GetBlockSpec extends MessageSpec[Block.BlockId] {
    override val messageCode: MessageCode = 22: Byte
    override val messageName: String = "GetBlock message"

    override def serializeData(signature: Block.BlockId): Array[Byte] = signature

    override def deserializeData(bytes: Array[Byte]): Try[Block.BlockId] = Try {
      require(bytes.length == EllipticCurveImpl.SignatureLength, "Data does not match length")
      bytes
    }
  }

  //todo: height removed, check the code using this message type
  object BlockMessageSpec extends MessageSpec[Block] {
    override val messageCode: MessageCode = 23: Byte

    override val messageName: String = "Block message"

    override def serializeData(block: Block): Array[Byte] = block.bytes

    override def deserializeData(bytes: Array[Byte]): Try[Block] = Block.parse(bytes)
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

  val specs = Seq(GetPeersSpec, PeersSpec, GetSignaturesSpec, SignaturesSpec,
    GetBlockSpec, BlockMessageSpec, ScoreMessageSpec)
}
