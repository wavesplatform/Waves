package com.wavesplatform.transaction

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.description._
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.Try

case class DataTransaction private (sender: PublicKey, data: List[DataEntry[_]], fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = DataTransaction
  override val assetFee: (Asset, Long)    = (Waves, fee)
  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce {
      Bytes.concat(
        Array(builder.typeId, version),
        sender,
        Shorts.toByteArray(data.size.toShort),
        Bytes.concat(data.view.map(_.toBytes): _*),
        Longs.toByteArray(timestamp),
        Longs.toByteArray(fee)
      )
    }

  implicit val dataItemFormat: Format[DataEntry[_]] = DataEntry.Format

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "data"    -> Json.toJson(data)
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 1
}

object DataTransaction extends TransactionParserFor[DataTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 12
  override val supportedVersions: Set[Byte] = Set(1)

  val MaxBytes      = 150 * 1024 // implicitly used for RIDE CONST_STRING and CONST_BYTESTR
  val MaxEntryCount = 100

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription
      .deserializeFromByteArray(bytes)
      .flatMap(
        validateTxContent(_, Some(bytes.length)).foldToTry
      )
  }

  def create(sender: PublicKey, data: List[DataEntry[_]], feeAmount: Long, timestamp: Long, proofs: Proofs): Either[ValidationError, TransactionT] = {

    val tx = DataTransaction(sender, data, feeAmount, timestamp, proofs)
    validateTxContent(tx)
  }

  private def validateTxContent(tx: DataTransaction, txByteCountOpt: Option[Int] = None): Either[ValidationError, TransactionT] = {
    if (tx.data.lengthCompare(MaxEntryCount) > 0 || tx.data.exists(!_.valid)) {
      Left(TxValidationError.TooBigArray)
    } else if (tx.data.exists(_.key.isEmpty)) {
      Left(TxValidationError.GenericError("Empty key found"))
    } else if (tx.data.map(_.key).distinct.lengthCompare(tx.data.size) < 0) {
      Left(TxValidationError.GenericError("Duplicate keys found"))
    } else if (tx.fee <= 0) {
      Left(TxValidationError.InsufficientFee())
    } else {
      Either.cond(txByteCountOpt.getOrElse(tx.bytes().length) <= MaxBytes, tx, TxValidationError.TooBigArray)
    }
  }

  def signed(sender: PublicKey,
             data: List[DataEntry[_]],
             feeAmount: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, data, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: KeyPair, data: List[DataEntry[_]], feeAmount: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, data, feeAmount, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[DataTransaction] = {
    (
      PublicKeyBytes(tailIndex(1), "Sender's public key"),
      ListDataEntryBytes(tailIndex(2)),
      LongBytes(tailIndex(3), "Timestamp"),
      LongBytes(tailIndex(4), "Fee"),
      ProofsBytes(tailIndex(5))
    ) mapN {
      case (senderPublicKey, data, timestamp, fee, proofs) =>
        DataTransaction(
          sender = senderPublicKey,
          data = data,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
