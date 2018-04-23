package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.mining.Miner.MaxTransactionsPerMicroblock
import com.wavesplatform.state.ByteStr
import scorex.account.PublicKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}
import scorex.network.message.Message._
import scorex.network.message._
import scorex.transaction.{Transaction, TransactionParsers}

import scala.util.Try

object GetPeersSpec extends MessageSpec[GetPeers.type] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val maxLength: Int = 0

  override def deserializeData(bytes: Array[Byte]): Try[GetPeers.type] =
    Try {
      require(bytes.isEmpty, "Non-empty data for GetPeers")
      GetPeers
    }

  override def serializeData(data: GetPeers.type): Array[Byte] = Array()
}

object PeersSpec extends MessageSpec[KnownPeers] {
  private val AddressLength = 4
  private val PortLength    = 4
  private val DataLength    = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val maxLength: Int = DataLength + 1000 * (AddressLength + PortLength)

  override def deserializeData(bytes: Array[Byte]): Try[KnownPeers] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
    val length      = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * (AddressLength + PortLength)), "Data does not match length")

    KnownPeers((0 until length).map { i =>
      val position     = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
      val address      = InetAddress.getByAddress(addressBytes)
      val portBytes    = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    })
  }

  override def serializeData(peers: KnownPeers): Array[Byte] = {
    val length      = peers.peers.size
    val lengthBytes = Ints.toByteArray(length)

    val xs = for {
      inetAddress <- peers.peers
      address     <- Option(inetAddress.getAddress)
    } yield (address.getAddress, inetAddress.getPort)

    xs.foldLeft(lengthBytes) {
      case (bs, (peerAddress, peerPort)) =>
        Bytes.concat(bs, peerAddress, Ints.toByteArray(peerPort))
    }
  }
}

trait SignaturesSeqSpec[A <: AnyRef] extends MessageSpec[A] {

  private val DataLength = 4

  def wrap(signatures: Seq[Array[Byte]]): A

  def unwrap(v: A): Seq[Array[Byte]]

  override val maxLength: Int = DataLength + (200 * SignatureLength)

  override def deserializeData(bytes: Array[Byte]): Try[A] = Try {
    val lengthBytes = bytes.take(DataLength)
    val length      = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * SignatureLength), "Data does not match length")

    wrap((0 until length).map { i =>
      val position = DataLength + (i * SignatureLength)
      bytes.slice(position, position + SignatureLength)
    })
  }

  override def serializeData(v: A): Array[Byte] = {
    val signatures  = unwrap(v)
    val length      = signatures.size
    val lengthBytes = Ints.toByteArray(length)

    //WRITE SIGNATURES
    signatures.foldLeft(lengthBytes) { case (bs, header) => Bytes.concat(bs, header) }
  }
}

object GetSignaturesSpec extends SignaturesSeqSpec[GetSignatures] {
  override def wrap(signatures: Seq[Array[Byte]]): GetSignatures = GetSignatures(signatures.map(ByteStr(_)))

  override def unwrap(v: GetSignatures): Seq[Array[MessageCode]] = v.signatures.map(_.arr)

  override val messageCode: MessageCode = 20: Byte
}

object SignaturesSpec extends SignaturesSeqSpec[Signatures] {
  override def wrap(signatures: Seq[Array[Byte]]): Signatures = Signatures(signatures.map(ByteStr(_)))

  override def unwrap(v: Signatures): Seq[Array[MessageCode]] = v.signatures.map(_.arr)

  override val messageCode: MessageCode = 21: Byte
}

object GetBlockSpec extends MessageSpec[GetBlock] {
  override val messageCode: MessageCode = 22: Byte

  override val maxLength: Int = SignatureLength

  override def serializeData(signature: GetBlock): Array[Byte] = signature.signature.arr

  override def deserializeData(bytes: Array[Byte]): Try[GetBlock] = Try {
    require(bytes.length == maxLength, "Data does not match length")
    GetBlock(ByteStr(bytes))
  }
}

object BlockSpec extends MessageSpec[Block] {
  override val messageCode: MessageCode = 23: Byte

  override val maxLength: Int = 271 + TransactionSpec.maxLength * Block.MaxTransactionsPerBlockVer3

  override def serializeData(block: Block): Array[Byte] = block.bytes()

  override def deserializeData(bytes: Array[Byte]): Try[Block] = Block.parseBytes(bytes)
}

object ScoreSpec extends MessageSpec[BigInt] {
  override val messageCode: MessageCode = 24: Byte

  override val maxLength: Int = 64 // allows representing scores as high as 6.6E153

  override def serializeData(score: BigInt): Array[Byte] = {
    val scoreBytes = score.toByteArray
    val bb         = java.nio.ByteBuffer.allocate(scoreBytes.length)
    bb.put(scoreBytes)
    bb.array()
  }

  override def deserializeData(bytes: Array[Byte]): Try[BigInt] = Try {
    BigInt(1, bytes)
  }
}

object CheckpointSpec extends MessageSpec[Checkpoint] {
  override val messageCode: MessageCode = 100: Byte

  private val HeightLength = Ints.BYTES

  override val maxLength: Int = 4 + Checkpoint.MaxCheckpoints * (HeightLength + SignatureLength)

  override def serializeData(checkpoint: Checkpoint): Array[Byte] =
    Bytes.concat(checkpoint.toSign, checkpoint.signature)

  override def deserializeData(bytes: Array[Byte]): Try[Checkpoint] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, Ints.BYTES)
    val length      = Ints.fromByteArray(lengthBytes)

    require(length <= Checkpoint.MaxCheckpoints)

    val items = (0 until length).map { i =>
      val position       = lengthBytes.length + (i * (HeightLength + SignatureLength))
      val heightBytes    = util.Arrays.copyOfRange(bytes, position, position + HeightLength)
      val height         = Ints.fromByteArray(heightBytes)
      val blockSignature = util.Arrays.copyOfRange(bytes, position + HeightLength, position + HeightLength + SignatureLength)
      BlockCheckpoint(height, blockSignature)
    }

    val signature = bytes.takeRight(SignatureLength)

    Checkpoint(items, signature)
  }
}

object TransactionSpec extends MessageSpec[Transaction] {
  override val messageCode: MessageCode = 25: Byte

  // Modeled after Data Transaction https://wavesplatform.atlassian.net/wiki/spaces/MAIN/pages/119734321/Data+Transaction
  override val maxLength: Int = 150000

  override def deserializeData(bytes: Array[Byte]): Try[Transaction] =
    TransactionParsers.parseBytes(bytes)

  override def serializeData(tx: Transaction): Array[Byte] = tx.bytes()
}

object MicroBlockInvSpec extends MessageSpec[MicroBlockInv] {
  override val messageCode: MessageCode = 26: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockInv] =
    Try(
      MicroBlockInv(
        sender = PublicKeyAccount.apply(bytes.take(KeyLength)),
        totalBlockSig = ByteStr(bytes.view.slice(KeyLength, KeyLength + SignatureLength).toArray),
        prevBlockSig = ByteStr(bytes.view.slice(KeyLength + SignatureLength, KeyLength + SignatureLength * 2).toArray),
        signature = ByteStr(bytes.view.slice(KeyLength + SignatureLength * 2, KeyLength + SignatureLength * 3).toArray)
      ))

  override def serializeData(inv: MicroBlockInv): Array[Byte] = {
    inv.sender.publicKey ++ inv.totalBlockSig.arr ++ inv.prevBlockSig.arr ++ inv.signature.arr
  }

  override val maxLength: Int = 300
}

object MicroBlockRequestSpec extends MessageSpec[MicroBlockRequest] {
  override val messageCode: MessageCode = 27: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockRequest] =
    Try(MicroBlockRequest(ByteStr(bytes)))

  override def serializeData(req: MicroBlockRequest): Array[Byte] = req.totalBlockSig.arr

  override val maxLength: Int = 500
}

object MicroBlockResponseSpec extends MessageSpec[MicroBlockResponse] {
  override val messageCode: MessageCode = 28: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockResponse] =
    MicroBlock.parseBytes(bytes).map(MicroBlockResponse)

  override def serializeData(resp: MicroBlockResponse): Array[Byte] = resp.microblock.bytes()

  override val maxLength: Int = 271 + TransactionSpec.maxLength * MaxTransactionsPerMicroblock

}

// Virtual, only for logs
object HandshakeSpec {
  val messageCode: MessageCode = 101: Byte
}

object BasicMessagesRepo {
  type Spec = MessageSpec[_ <: AnyRef]

  val specs: Seq[Spec] = Seq(
    GetPeersSpec,
    PeersSpec,
    GetSignaturesSpec,
    SignaturesSpec,
    GetBlockSpec,
    BlockSpec,
    ScoreSpec,
    CheckpointSpec,
    TransactionSpec,
    MicroBlockInvSpec,
    MicroBlockRequestSpec,
    MicroBlockResponseSpec
  )

  val specsByCodes: Map[Byte, Spec]       = specs.map(s => s.messageCode  -> s).toMap
  val specsByClasses: Map[Class[_], Spec] = specs.map(s => s.contentClass -> s).toMap
}
