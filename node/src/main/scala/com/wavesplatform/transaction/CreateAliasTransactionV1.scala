package com.wavesplatform.transaction

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class CreateAliasTransactionV1 private (sender: PublicKey, alias: Alias, fee: Long, timestamp: Long, signature: ByteStr)
    extends CreateAliasTransaction
    with SignedTransaction {

  override def version: Byte = 1

  override val builder: TransactionParser     = CreateAliasTransactionV1
  override val id: Coeval[ByteStr]            = Coeval.evalOnce(ByteStr(crypto.fastHash(builder.typeId +: alias.bytes.arr)))
  override val bodyBytes: Coeval[Array[Byte]] = baseBytes.map(base => Bytes.concat(Array(builder.typeId), base))
  override val bytes: Coeval[Array[Byte]]     = bodyBytes.map(body => Bytes.concat(body, signature.arr))
}

object CreateAliasTransactionV1 extends TransactionParserFor[CreateAliasTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = CreateAliasTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.fee > 0, tx, TxValidationError.InsufficientFee)
        .foldToTry
    }
  }

  def create(sender: PublicKey, alias: Alias, fee: Long, timestamp: Long, signature: ByteStr): Either[ValidationError, TransactionT] = {
    if (fee <= 0) {
      Left(TxValidationError.InsufficientFee())
    } else {
      Right(CreateAliasTransactionV1(sender, alias, fee, timestamp, signature))
    }
  }

  def signed(sender: PublicKey, alias: Alias, fee: Long, timestamp: Long, signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, alias, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: KeyPair, alias: Alias, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, alias, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[CreateAliasTransactionV1] = {
    (
      PublicKeyBytes(tailIndex(1), "Sender's public key"),
      AliasBytes(tailIndex(2), "Alias object"),
      LongBytes(tailIndex(3), "Fee"),
      LongBytes(tailIndex(4), "Timestamp"),
      SignatureBytes(tailIndex(5), "Signature")
    ) mapN CreateAliasTransactionV1.apply
  }
}
