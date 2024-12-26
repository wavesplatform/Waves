package com.wavesplatform.network

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.serialization.MicroBlockSerializer
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto.*
import com.wavesplatform.mining.Miner.MaxTransactionsPerMicroblock
import com.wavesplatform.mining.MiningConstraints
import com.wavesplatform.network.message.*
import com.wavesplatform.network.message.Message.*
import com.wavesplatform.protobuf.block.{PBBlock, PBBlocks, PBMicroBlocks, SignedMicroBlock}
import com.wavesplatform.protobuf.snapshot.{BlockSnapshot as PBBlockSnapshot, MicroBlockSnapshot as PBMicroBlockSnapshot}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.transaction.{DataTransaction, EthereumTransaction, Transaction, TransactionParsers}

import java.net.{InetAddress, InetSocketAddress}
import java.util
import scala.reflect.ClassTag
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

abstract class InetSocketAddressSeqSpec[A <: AnyRef : ClassTag] extends MessageSpec[A] {
  private val AddressLength = 4
  private val PortLength    = 4
  private val DataLength    = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val maxLength: Int = DataLength + 1000 * (AddressLength + PortLength)

  protected def unwrap(v: A): Seq[InetSocketAddress]
  protected def wrap(addresses: Seq[InetSocketAddress]): A

  override def deserializeData(bytes: Array[Byte]): Try[A] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
    val length      = Ints.fromByteArray(lengthBytes)

    val expectedMessageLength = DataLength + (length * (AddressLength + PortLength))
    assert(
      bytes.length == expectedMessageLength,
      s"Invalid KnownPeers message length ${bytes.length}, expecting $expectedMessageLength for $length peers"
    )

    (0 until length).map { i =>
      val position     = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
      val address      = InetAddress.getByAddress(addressBytes)
      val portBytes    = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    }
  }.map(wrap)

  def serializeData(value: A): Array[Byte] = {
    val peersWithResolvedAddresses = for {
      inetAddress <- unwrap(value)
      address     <- Option(inetAddress.getAddress)
    } yield (address.getAddress, inetAddress.getPort)

    peersWithResolvedAddresses.foldLeft(Ints.toByteArray(peersWithResolvedAddresses.size)) { case (bs, (peerAddress, peerPort)) =>
      Bytes.concat(bs, peerAddress, Ints.toByteArray(peerPort))
    }
  }
}

object PeersSpec extends InetSocketAddressSeqSpec[KnownPeers] {
  override protected def unwrap(v: KnownPeers): Seq[InetSocketAddress] = v.peers

  override protected def wrap(addresses: Seq[InetSocketAddress]): KnownPeers = KnownPeers(addresses)
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
    Bytes.concat((Ints.toByteArray(unwrap(v).length) +: unwrap(v))*)
  }
}

trait BlockIdSeqSpec[A <: AnyRef] extends MessageSpec[A] {
  def wrap(blockIds: Seq[Array[Byte]]): A

  def unwrap(v: A): Seq[Array[Byte]]

  override val maxLength: Int = Ints.BYTES + (200 * SignatureLength) + 200

  override def deserializeData(bytes: Array[Byte]): Try[A] = Try {
    val lengthBytes = bytes.take(Ints.BYTES)
    val length      = Ints.fromByteArray(lengthBytes)

    require(bytes.length <= Ints.BYTES + (length * SignatureLength) + length, "Data does not match length")

    val (_, arrays) = (0 until length).foldLeft((Ints.BYTES, Seq.empty[Array[Byte]])) { case ((pos, arrays), _) =>
      val length = bytes(pos)
      val result = bytes.slice(pos + 1, pos + 1 + length)
      require(result.length == length, "Data does not match length")
      (pos + length + 1, arrays :+ result)
    }
    wrap(arrays)
  }

  override def serializeData(v: A): Array[Byte] = {
    val signatures  = unwrap(v)
    val length      = signatures.size
    val lengthBytes = Ints.toByteArray(length)

    signatures.foldLeft(lengthBytes) { case (bs, sig) =>
      Bytes.concat(bs, Array(sig.length.ensuring(_.isValidByte).toByte), sig)
    }
  }
}

object GetSignaturesSpec extends SignaturesSeqSpec[GetSignatures] {
  def isSupported(signatures: Seq[ByteStr]): Boolean             = signatures.forall(_.arr.length == SignatureLength)
  override def wrap(signatures: Seq[Array[Byte]]): GetSignatures = GetSignatures(signatures.map(ByteStr(_)))
  override def unwrap(v: GetSignatures): Seq[Array[MessageCode]] = v.signatures.map(_.arr)
  override val messageCode: MessageCode                          = 20: Byte
}

object SignaturesSpec extends SignaturesSeqSpec[Signatures] {
  override def wrap(signatures: Seq[Array[Byte]]): Signatures = Signatures(signatures.map(ByteStr(_)))
  override def unwrap(v: Signatures): Seq[Array[Byte]]        = v.signatures.map(_.arr)
  override val messageCode: MessageCode                       = 21: Byte
}

object GetBlockIdsSpec extends BlockIdSeqSpec[GetSignatures] {
  override def wrap(blockIds: Seq[Array[Byte]]): GetSignatures   = GetSignatures(blockIds.map(ByteStr(_)))
  override def unwrap(v: GetSignatures): Seq[Array[MessageCode]] = v.signatures.map(_.arr)
  override val messageCode: MessageCode                          = 32: Byte
}

object BlockIdsSpec extends BlockIdSeqSpec[Signatures] {
  override def wrap(blockIds: Seq[Array[Byte]]): Signatures = Signatures(blockIds.map(ByteStr(_)))
  override def unwrap(v: Signatures): Seq[Array[Byte]]      = v.signatures.map(_.arr)
  override val messageCode: MessageCode                     = 33: Byte
}

object GetBlockSpec extends MessageSpec[GetBlock] {
  override val messageCode: MessageCode = 22: Byte

  override val maxLength: Int = SignatureLength

  override def serializeData(signature: GetBlock): Array[Byte] = signature.signature.arr

  override def deserializeData(bytes: Array[Byte]): Try[GetBlock] = Try {
    require(Block.validateReferenceLength(bytes.length), "Data does not match length")
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

object TransactionSpec extends MessageSpec[Transaction] {
  override val messageCode: MessageCode = 25: Byte

  // Modeled after Data Transaction https://wavesplatform.atlassian.net/wiki/spaces/MAIN/pages/119734321/Data+Transaction
  override val maxLength: Int = (DataTransaction.MaxBytes * 1.2).toInt // 150 * 1024

  override def deserializeData(bytes: Array[Byte]): Try[Transaction] =
    TransactionParsers.parseBytes(bytes)

  override def serializeData(tx: Transaction): Array[Byte] =
    tx.bytes().ensuring(!tx.isInstanceOf[EthereumTransaction])
}

object MicroBlockInvSpec extends MessageSpec[MicroBlockInv] {
  override val messageCode: MessageCode = 26: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockInv] =
    Try(
      bytes.length match {
        case l if l == (KeyLength + SignatureLength * 3) =>
          MicroBlockInv(
            sender = PublicKey.apply(bytes.take(KeyLength)),
            totalBlockId = ByteStr(bytes.view.slice(KeyLength, KeyLength + SignatureLength).toArray),
            reference = ByteStr(bytes.view.slice(KeyLength + SignatureLength, KeyLength + SignatureLength * 2).toArray),
            signature = ByteStr(bytes.view.slice(KeyLength + SignatureLength * 2, KeyLength + SignatureLength * 3).toArray)
          )

        case l if l == (KeyLength + (DigestLength * 2) + SignatureLength) =>
          MicroBlockInv(
            sender = PublicKey.apply(bytes.take(KeyLength)),
            totalBlockId = ByteStr(bytes.view.slice(KeyLength, KeyLength + DigestLength).toArray),
            reference = ByteStr(bytes.view.slice(KeyLength + DigestLength, KeyLength + DigestLength * 2).toArray),
            signature = ByteStr(bytes.view.slice(KeyLength + DigestLength * 2, KeyLength + (DigestLength * 2) + SignatureLength).toArray)
          )
      }
    )

  override def serializeData(inv: MicroBlockInv): Array[Byte] =
    inv.sender.arr ++ inv.totalBlockId.arr ++ inv.reference.arr ++ inv.signature.arr

  override val maxLength: Int = 300
}

object MicroBlockRequestSpec extends MessageSpec[MicroBlockRequest] {
  override val messageCode: MessageCode = 27: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockRequest] =
    Try(MicroBlockRequest(ByteStr(bytes)))

  override def serializeData(req: MicroBlockRequest): Array[Byte] = req.totalBlockSig.arr

  override val maxLength: Int = 500
}

object LegacyMicroBlockResponseSpec extends MessageSpec[MicroBlockResponse] {
  override val messageCode: MessageCode = 28: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockResponse] =
    MicroBlock.parseBytes(bytes).map(MicroBlockResponse(_))

  override def serializeData(resp: MicroBlockResponse): Array[Byte] = {
    require(resp.microblock.version < Block.ProtoBlockVersion)
    MicroBlockSerializer.toBytes(resp.microblock)
  }

  override val maxLength: Int = 271 + TransactionSpec.maxLength * MaxTransactionsPerMicroblock
}

object PBBlockSpec extends MessageSpec[Block] {
  override val messageCode: MessageCode = 29: Byte

  // BlockHeader + signature + max transactions size + max proto serialization meta + some gap
  override val maxLength: Int = 461 + 64 + MiningConstraints.MaxTxsSizeInBytes + 37117 + 100

  override def deserializeData(bytes: Array[Byte]): Try[Block] = PBBlocks.vanilla(PBBlock.parseFrom(bytes))

  override def serializeData(data: Block): Array[Byte] = PBBlocks.protobuf(data).toByteArray
}

object PBMicroBlockSpec extends MessageSpec[MicroBlockResponse] {
  override val messageCode: MessageCode = 30: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockResponse] =
    PBMicroBlocks.vanilla(SignedMicroBlock.parseFrom(bytes))

  override def serializeData(resp: MicroBlockResponse): Array[Byte] =
    PBMicroBlocks.protobuf(resp.microblock, resp.totalBlockId).toByteArray

  override val maxLength: Int = PBBlockSpec.maxLength + crypto.DigestLength
}

object PBTransactionSpec extends MessageSpec[Transaction] {
  override val messageCode: MessageCode = 31: Byte

  // 624 + DataTransaction.MaxProtoBytes + 5 + 100 // Signed (8 proofs) PBTransaction + max DataTransaction.DataEntry + max proto serialization meta + gap
  override val maxLength: Int = (DataTransaction.MaxBytes * 1.2).toInt

  override def deserializeData(bytes: Array[MessageCode]): Try[Transaction] =
    PBTransactions.tryToVanilla(PBSignedTransaction.parseFrom(bytes))

  override def serializeData(data: Transaction): Array[MessageCode] =
    PBTransactions.toByteArray(data)
}

object GetSnapsnotSpec extends MessageSpec[GetSnapshot] {
  override val messageCode: MessageCode = 34: Byte

  override val maxLength: Int = SignatureLength

  override def serializeData(msg: GetSnapshot): Array[Byte] = msg.blockId.arr

  override def deserializeData(bytes: Array[Byte]): Try[GetSnapshot] = Try {
    require(Block.validateReferenceLength(bytes.length), "Data does not match length")
    GetSnapshot(ByteStr(bytes))
  }
}

object MicroSnapshotRequestSpec extends MessageSpec[MicroSnapshotRequest] {
  override val messageCode: MessageCode = 35: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroSnapshotRequest] =
    Try(MicroSnapshotRequest(ByteStr(bytes)))

  override def serializeData(req: MicroSnapshotRequest): Array[Byte] = req.totalBlockId.arr

  override val maxLength: Int = SignatureLength
}

object BlockSnapshotResponseSpec extends MessageSpec[BlockSnapshotResponse] {
  override val messageCode: MessageCode = 36: Byte

  override def deserializeData(bytes: Array[Byte]): Try[BlockSnapshotResponse] =
    Try(BlockSnapshotResponse.fromProtobuf(PBBlockSnapshot.parseFrom(bytes)))

  override def serializeData(data: BlockSnapshotResponse): Array[Byte] = data.toProtobuf.toByteArray

  override val maxLength: Int = NetworkServer.MaxFrameLength
}

object MicroBlockSnapshotResponseSpec extends MessageSpec[MicroBlockSnapshotResponse] {
  override val messageCode: MessageCode = 37: Byte

  override def deserializeData(bytes: Array[Byte]): Try[MicroBlockSnapshotResponse] =
    Try(MicroBlockSnapshotResponse.fromProtobuf(PBMicroBlockSnapshot.parseFrom(bytes)))

  override def serializeData(data: MicroBlockSnapshotResponse): Array[Byte] = data.toProtobuf.toByteArray

  override val maxLength: Int = NetworkServer.MaxFrameLength
}

// Virtual, only for logs
object HandshakeSpec {
  val messageCode: MessageCode = 101: Byte
}

object BasicMessagesRepo {
  type Spec = MessageSpec[? <: AnyRef]

  private val specs: Seq[Spec] = Seq(
    GetPeersSpec,
    PeersSpec,
    GetSignaturesSpec,
    SignaturesSpec,
    GetBlockSpec,
    BlockSpec,
    ScoreSpec,
    TransactionSpec,
    MicroBlockInvSpec,
    MicroBlockRequestSpec,
    LegacyMicroBlockResponseSpec,
    PBBlockSpec,
    PBMicroBlockSpec,
    PBTransactionSpec,
    GetBlockIdsSpec,
    BlockIdsSpec,
    GetSnapsnotSpec,
    MicroSnapshotRequestSpec,
    BlockSnapshotResponseSpec,
    MicroBlockSnapshotResponseSpec
  )

  val specsByCodes: Map[Byte, Spec]       = specs.map(s => s.messageCode -> s).toMap
  val specsByClasses: Map[Class[?], Spec] = specs.map(s => s.contentClass -> s).toMap
}
