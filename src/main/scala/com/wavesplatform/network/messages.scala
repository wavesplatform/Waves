package com.wavesplatform.network

import java.net.InetSocketAddress

import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, MicroBlock}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.{History, Signed}


sealed trait Message

case object GetPeers extends Message
case class KnownPeers(peers: Seq[InetSocketAddress]) extends Message
case class GetSignatures(signatures: Seq[ByteStr]) extends Message
case class Signatures(signatures: Seq[ByteStr]) extends Message
case class GetBlock(signature: ByteStr) extends Message
case class LocalScoreChanged(newLocalScore: History.BlockchainScore) extends Message
case class RawBytes(code: Byte, data: Array[Byte]) extends Message
case class BlockForged(block: Block) extends Message
case class MicroBlockRequest(totalBlockSig: ByteStr)  extends Message
case class MicroBlockResponse(microblock: MicroBlock) extends Message
case class LoadBlockchainExtension(lastBlockIds: Seq[ByteStr])
case class ExtensionIds(lastCommonId: ByteStr, extensionIds: Seq[ByteStr])
case class ExtensionBlocks(extension: Seq[Block])

case class MicroBlockInv(sender: PublicKeyAccount, totalBlockSig: ByteStr, prevBlockSig: ByteStr, signature: ByteStr) extends Message with Signed {
  override protected val signatureValid = Coeval.evalOnce(EllipticCurveImpl.verify(signature.arr, sender.toAddress.bytes.arr ++ totalBlockSig.arr ++ prevBlockSig.arr, sender.publicKey))
  override def toString: String = s"MicroBlockInv(${totalBlockSig.trim} ~> ${prevBlockSig.trim})"
}
object MicroBlockInv{

  def apply(sender: PrivateKeyAccount, totalBlockSig : ByteStr, prevBlockSig: ByteStr): MicroBlockInv = {
    val signature = EllipticCurveImpl.sign(sender, sender.toAddress.bytes.arr ++ totalBlockSig.arr ++ prevBlockSig.arr)
    new MicroBlockInv(sender, totalBlockSig, prevBlockSig, ByteStr(signature))
  }
}