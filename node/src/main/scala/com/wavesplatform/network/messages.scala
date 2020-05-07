package com.wavesplatform.network

import java.net.InetSocketAddress
import java.util

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.transaction.{LegacyPBSwitch, ProtobufOnly, Signed, Transaction}
import monix.eval.Coeval

sealed trait Message

case object GetPeers extends Message

case class KnownPeers(peers: Seq[InetSocketAddress]) extends Message

case class GetSignatures(signatures: Seq[ByteStr]) extends Message {
  override def toString: String = s"GetSignatures(${formatSignatures(signatures)})"
}

case class Signatures(signatures: Seq[ByteStr]) extends Message {
  override def toString: String = s"Signatures(${formatSignatures(signatures)})"
}

case class GetBlock(signature: ByteStr) extends Message

case class LocalScoreChanged(newLocalScore: BigInt) extends Message

case class RawBytes(code: Byte, data: Array[Byte]) extends Message {
  override def toString: String = s"RawBytes($code, ${data.length} bytes)"

  override def equals(obj: Any): Boolean = obj match {
    case o: RawBytes => o.code == code && util.Arrays.equals(o.data, data)
    case _ => false
  }
}

object RawBytes {
  def fromTransaction(tx: Transaction): RawBytes = tx match {
    case p: LegacyPBSwitch if p.isProtobufVersion => RawBytes(PBTransactionSpec.messageCode, PBTransactionSpec.serializeData(tx))
    case _: ProtobufOnly                          => RawBytes(PBTransactionSpec.messageCode, PBTransactionSpec.serializeData(tx))
    case tx                                       => RawBytes(TransactionSpec.messageCode, TransactionSpec.serializeData(tx))
  }

  def fromBlock(b: Block): RawBytes =
    if (b.header.version < Block.ProtoBlockVersion) RawBytes(BlockSpec.messageCode, BlockSpec.serializeData(b))
    else RawBytes(PBBlockSpec.messageCode, PBBlockSpec.serializeData(b))

  def fromMicroBlock(mb: MicroBlockResponse): RawBytes =
    if (mb.microblock.version < Block.ProtoBlockVersion)
      RawBytes(LegacyMicroBlockResponseSpec.messageCode, LegacyMicroBlockResponseSpec.serializeData(mb))
    else RawBytes(PBMicroBlockSpec.messageCode, PBMicroBlockSpec.serializeData(mb))
}

case class BlockForged(block: Block) extends Message

case class MicroBlockRequest(totalBlockSig: ByteStr) extends Message

case class MicroBlockResponse(microblock: MicroBlock, totalBlockId: BlockId)

object MicroBlockResponse {
  def apply(mb: MicroBlock): MicroBlockResponse = {
    require(mb.version < Block.ProtoBlockVersion)
    MicroBlockResponse(mb, mb.totalResBlockSig)
  }
}

case class MicroBlockInv(sender: PublicKey, totalBlockId: ByteStr, reference: ByteStr, signature: ByteStr) extends Message with Signed {
  override val signatureValid: Coeval[Boolean] =
    Coeval.evalOnce(crypto.verify(signature, sender.toAddress.bytes ++ totalBlockId.arr ++ reference.arr, sender))

  override def toString: String = s"MicroBlockInv(${totalBlockId.trim} ~> ${reference.trim})"
}

object MicroBlockInv {

  def apply(sender: KeyPair, totalBlockRef: ByteStr, prevBlockRef: ByteStr): MicroBlockInv = {
    val signature = crypto.sign(sender.privateKey, sender.toAddress.bytes ++ totalBlockRef.arr ++ prevBlockRef.arr)
    new MicroBlockInv(sender.publicKey, totalBlockRef, prevBlockRef, signature)
  }
}
