package com.wavesplatform.block

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block.{BlockId, transParseBytes}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
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

case class MicroBlock(version: Byte,
                      sender: PublicKey,
                      transactionData: Seq[Transaction],
                      prevResBlockSig: BlockId,
                      totalResBlockSig: BlockId,
                      signature: ByteStr)
    extends Signed {

  private val versionField: ByteBlockField          = ByteBlockField("version", version)
  private val prevResBlockSigField: BlockIdField    = BlockIdField("prevResBlockSig", prevResBlockSig.arr)
  private val totalResBlockSigField: BlockIdField   = BlockIdField("totalResBlockSigField", totalResBlockSig.arr)
  private val signerDataField: SignerDataBlockField = SignerDataBlockField("signature", SignerData(sender, signature))
  private val transactionDataField                  = TransactionsBlockField(version.toInt, transactionData)

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val txBytesSize = transactionDataField.bytes().length
    val txBytes     = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionDataField.bytes()

    versionField.bytes() ++
      prevResBlockSigField.bytes() ++
      totalResBlockSigField.bytes() ++
      txBytes ++
      signerDataField.bytes()
  }

  private val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(bytes().dropRight(SignatureLength))

  override val signatureValid: Coeval[Boolean]        = Coeval.evalOnce(crypto.verify(signature.arr, bytesWithoutSignature(), sender))
  override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  override def toString: String = s"MicroBlock(${totalResBlockSig.trim} -> ${prevResBlockSig.trim}, txs=${transactionData.size})"
}

object MicroBlock extends ScorexLogging {
  private def create(version: Byte,
                     generator: PublicKey,
                     transactionData: Seq[Transaction],
                     prevResBlockSig: BlockId,
                     totalResBlockSig: BlockId,
                     signature: ByteStr): Either[ValidationError, MicroBlock] = {
    if (transactionData.isEmpty)
      Left(GenericError("cannot create empty MicroBlock"))
    else if (transactionData.size > MaxTransactionsPerMicroblock)
      Left(GenericError(s"too many txs in MicroBlock: allowed: $MaxTransactionsPerMicroblock, actual: ${transactionData.size}"))
    else
      Right(new MicroBlock(version, generator, transactionData, prevResBlockSig, totalResBlockSig, signature))
  }

  def buildAndSign(generator: KeyPair,
                   transactionData: Seq[Transaction],
                   prevResBlockSig: BlockId,
                   totalResBlockSig: BlockId): Either[ValidationError, MicroBlock] =
    for {
      _ <- Either.cond(prevResBlockSig.arr.length == SignatureLength, (), GenericError(s"Incorrect prevResBlockSig: ${prevResBlockSig.arr.length}"))
      _ <- Either.cond(totalResBlockSig.arr.length == SignatureLength,
                       (),
                       GenericError(s"Incorrect totalResBlockSig: ${totalResBlockSig.arr.length}"))
      _         <- Either.cond(generator.publicKey.length == KeyLength, (), GenericError(s"Incorrect generator.publicKey: ${generator.publicKey.length}"))
      nonSigned <- create(version = 3: Byte, generator, transactionData, prevResBlockSig, totalResBlockSig, ByteStr.empty)
    } yield {
      val toSign    = nonSigned.bytes
      val signature = crypto.sign(generator, toSign())
      nonSigned.copy(signature = ByteStr(signature))
    }

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    Try {

      val version = bytes.head

      var position = 1

      val prevResBlockSig = ByteStr(bytes.slice(position, position + SignatureLength))
      position += SignatureLength

      val totalResBlockSig = ByteStr(bytes.slice(position, position + SignatureLength))
      position += SignatureLength

      val tBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4
      val tBytes       = bytes.slice(position, position + tBytesLength)
      val txBlockField = transParseBytes(version, tBytes).get
      position += tBytesLength

      val genPK = bytes.slice(position, position + KeyLength)
      position += KeyLength

      val signature = ByteStr(bytes.slice(position, position + SignatureLength))

      create(version, PublicKey(genPK), txBlockField, prevResBlockSig, totalResBlockSig, signature).explicitGet()
    }.recoverWith {
      case t: Throwable =>
        log.error("Error when parsing microblock", t)
        Failure(t)
    }
}
