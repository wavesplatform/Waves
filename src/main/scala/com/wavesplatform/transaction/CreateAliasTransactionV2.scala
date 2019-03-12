package com.wavesplatform.transaction

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{Alias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.transaction.description._
import monix.eval.Coeval
import com.wavesplatform.account.{Alias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import monix.eval.Coeval

import scala.util.Try

final case class CreateAliasTransactionV2 private (sender: PublicKeyAccount, alias: Alias, fee: Long, timestamp: Long, proofs: Proofs)
    extends CreateAliasTransaction {

  override val id: Coeval[ByteStr]            = Coeval.evalOnce(ByteStr(crypto.fastHash(builder.typeId +: alias.bytes.arr)))
  override val bodyBytes: Coeval[Array[Byte]] = baseBytes.map(base => Bytes.concat(Array(builder.typeId, version), base))
  override val bytes: Coeval[Array[Byte]]     = (bodyBytes, proofs.bytes).mapN { case (bb, pb) => Bytes.concat(Array(0: Byte), bb, pb) }

  override def builder: TransactionParser = CreateAliasTransactionV2
  override def version: Byte              = 2
}

object CreateAliasTransactionV2 extends TransactionParserFor[CreateAliasTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte = CreateAliasTransaction.typeId

  override def supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(bytes: Array[Byte]): Try[CreateAliasTransactionV2] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.fee > 0, tx, ValidationError.InsufficientFee)
        .foldToTry
    }
  }

  def create(sender: PublicKeyAccount,
             alias: Alias,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, CreateAliasTransactionV2] = {
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(CreateAliasTransactionV2(sender, alias, fee, timestamp, proofs))
    }
  }

  def signed(sender: PublicKeyAccount,
             alias: Alias,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, CreateAliasTransactionV2] = {
    for {
      unsigned <- create(sender, alias, fee, timestamp, Proofs.empty)
      proofs   <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes()))))
    } yield unsigned.copy(proofs = proofs)
  }

  def selfSigned(sender: PrivateKeyAccount, alias: Alias, fee: Long, timestamp: Long): Either[ValidationError, CreateAliasTransactionV2] = {
    signed(sender, alias, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[CreateAliasTransactionV2] = {
    (
      PublicKeyAccountBytes(tailIndex(1), "Sender's public key"),
      AliasBytes(tailIndex(2), "Alias object"),
      LongBytes(tailIndex(3), "Fee"),
      LongBytes(tailIndex(4), "Timestamp"),
      ProofsBytes(tailIndex(5))
    ) mapN CreateAliasTransactionV2.apply
  }
}
