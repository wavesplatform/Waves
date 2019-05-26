package com.wavesplatform.network

import java.net.InetSocketAddress

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.transaction.{Signed, Transaction}
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

case class RawBytes(code: Byte, data: Array[Byte]) extends Message

object RawBytes {
  def from(tx: Transaction): RawBytes = RawBytes(TransactionSpec.messageCode, tx.bytes())
  def from(b: Block): RawBytes        = RawBytes(BlockSpec.messageCode, b.bytes())
}

case class BlockForged(block: Block) extends Message

case class MicroBlockRequest(totalBlockSig: ByteStr) extends Message

case class MicroBlockResponse(microblock: MicroBlock) extends Message

case class MicroBlockInv(sender: PublicKey, totalBlockSig: ByteStr, prevBlockSig: ByteStr, signature: ByteStr) extends Message with Signed {
  override val signatureValid: Coeval[Boolean] =
    Coeval.evalOnce(crypto.verify(signature.arr, sender.toAddress.bytes.arr ++ totalBlockSig.arr ++ prevBlockSig.arr, sender))

  override def toString: String = s"MicroBlockInv(${totalBlockSig.trim} ~> ${prevBlockSig.trim})"
}

object MicroBlockInv {

  def apply(sender: KeyPair, totalBlockSig: ByteStr, prevBlockSig: ByteStr): MicroBlockInv = {
    val signature = crypto.sign(sender, sender.toAddress.bytes.arr ++ totalBlockSig.arr ++ prevBlockSig.arr)
    new MicroBlockInv(sender, totalBlockSig, prevBlockSig, ByteStr(signature))
  }
}
