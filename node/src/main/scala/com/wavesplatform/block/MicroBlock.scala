package com.wavesplatform.block

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.serialization.MicroBlockSerializer
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.Miner.MaxTransactionsPerMicroblock
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval

import scala.util.{Failure, Try}

case class MicroBlock(
    version: Byte,
    sender: PublicKey,
    transactionData: Seq[Transaction],
    prevResBlockSig: BlockId,
    totalResBlockSig: BlockId,
    signature: ByteStr
) extends Signed {

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(MicroBlockSerializer.toBytes(this))

  private val bytesToSign: Coeval[Array[Byte]] = Coeval.evalOnce(bytes().dropRight(SignatureLength))

  override val signatureValid: Coeval[Boolean]        = Coeval.evalOnce(crypto.verify(signature.arr, bytesToSign(), sender))
  override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  override def toString: String = s"MicroBlock(${totalResBlockSig.trim} -> ${prevResBlockSig.trim}, txs=${transactionData.size})"
}

object MicroBlock extends ScorexLogging {

  def buildAndSign(
      generator: KeyPair,
      transactionData: Seq[Transaction],
      prevResBlockSig: BlockId,
      totalResBlockSig: BlockId
  ): Either[ValidationError, MicroBlock] =
    // format: off
    for {
      _         <- Either.cond(prevResBlockSig.arr.length == SignatureLength, (), GenericError(s"Incorrect prevResBlockSig: ${prevResBlockSig.arr.length}"))
      _         <- Either.cond(totalResBlockSig.arr.length == SignatureLength, (), GenericError(s"Incorrect totalResBlockSig: ${totalResBlockSig.arr.length}"))
      _         <- Either.cond(generator.publicKey.length == KeyLength, (), GenericError(s"Incorrect generator.publicKey: ${generator.publicKey.length}"))
      nonSigned <- validate(MicroBlock(version = 3.toByte, generator, transactionData, prevResBlockSig, totalResBlockSig, ByteStr.empty))
    } yield {
      val toSign    = nonSigned.bytes
      val signature = crypto.sign(generator, toSign())
      nonSigned.copy(signature = ByteStr(signature))
    }
    // format: on

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    MicroBlockSerializer
      .parseBytes(bytes)
      .flatMap(validate(_).asTry)
      .recoverWith {
        case t: Throwable =>
          log.error("Error when parsing microblock", t)
          Failure(t)
      }

  private def validate(block: MicroBlock): Either[ValidationError, MicroBlock] =
    if (block.transactionData.isEmpty)
      Left(GenericError("cannot create empty MicroBlock"))
    else if (block.transactionData.size > MaxTransactionsPerMicroblock)
      Left(GenericError(s"too many txs in MicroBlock: allowed: $MaxTransactionsPerMicroblock, actual: ${block.transactionData.size}"))
    else
      Right(block)
}
