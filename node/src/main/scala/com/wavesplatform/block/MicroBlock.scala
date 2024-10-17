package com.wavesplatform.block

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.serialization.MicroBlockSerializer
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.*
import monix.eval.Coeval

import scala.util.Try

case class MicroBlock(
    version: Byte,
    sender: PublicKey,
    transactionData: Seq[Transaction],
    reference: BlockId,
    totalResBlockSig: ByteStr,
    signature: ByteStr,
    stateHash: Option[ByteStr]
) extends Signed {
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(MicroBlockSerializer.toBytes(this))

  private[block] val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(copy(signature = ByteStr.empty).bytes())

  override val signatureValid: Coeval[Boolean]        = Coeval.evalOnce(crypto.verify(signature, bytesWithoutSignature(), sender))
  override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  override def toString: String = s"MicroBlock(... -> ${reference.trim}, txs=${transactionData.size}"

  def stringRepr(totalBlockId: ByteStr): String = s"MicroBlock(${totalBlockId.trim} -> ${reference.trim}, txs=${transactionData.size})"
}

object MicroBlock {
  def buildAndSign(
      version: Byte,
      generator: KeyPair,
      transactionData: Seq[Transaction],
      reference: BlockId,
      totalResBlockSig: BlockId,
      stateHash: Option[ByteStr]
  ): Either[ValidationError, MicroBlock] =
    MicroBlock(version, generator.publicKey, transactionData, reference, totalResBlockSig, ByteStr.empty, stateHash).validate
      .map(_.sign(generator.privateKey))

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    MicroBlockSerializer
      .parseBytes(bytes)
      .flatMap(_.validateToTry)

  def validateReferenceLength(version: Byte, length: Int): Boolean =
    length == Block.referenceLength(version)
}
