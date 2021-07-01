package com.wavesplatform.block

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.serialization.MicroBlockSerializer
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.PBMicroBlocks
import com.wavesplatform.state._
import com.wavesplatform.transaction._
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval

import scala.util.{Failure, Try}

case class MicroBlock(
    version: Byte,
    sender: PublicKey,
    transactionData: Seq[Transaction],
    reference: BlockId,
    totalResBlockSig: BlockId,
    signature: ByteStr
) extends Signed {
  private[block] val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (version < Block.ProtoBlockVersion) MicroBlockSerializer.toBytes(copy(signature = ByteStr.empty))
    else PBMicroBlocks.protobuf(this, totalResBlockSig).getMicroBlock.toByteArray
  )

  override val signatureValid: Coeval[Boolean]        = Coeval.evalOnce(crypto.verify(signature, bytesWithoutSignature(), sender))
  override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  override def toString: String = s"MicroBlock(... -> ${reference.trim}, txs=${transactionData.size}"

  def stringRepr(totalBlockId: ByteStr): String = s"MicroBlock(${totalBlockId.trim} -> ${reference.trim}, txs=${transactionData.size})"
}

object MicroBlock extends ScorexLogging {
  def buildAndSign(
      version: Byte,
      generator: KeyPair,
      transactionData: Seq[Transaction],
      reference: BlockId,
      totalResBlockSig: BlockId
  ): Either[ValidationError, MicroBlock] =
    MicroBlock(version, generator.publicKey, transactionData, reference, totalResBlockSig, ByteStr.empty).validate
      .map(_.sign(generator.privateKey))

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    MicroBlockSerializer
      .parseBytes(bytes)
      .flatMap(_.validateToTry)
      .recoverWith {
        case t: Throwable =>
          log.error("Error when parsing microblock", t)
          Failure(t)
      }

  def validateReferenceLength(version: Byte, length: Int): Boolean =
    length == Block.referenceLength(version)
}
