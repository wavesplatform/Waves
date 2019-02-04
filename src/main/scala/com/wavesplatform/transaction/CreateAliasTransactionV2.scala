package com.wavesplatform.transaction

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{Alias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

final case class CreateAliasTransactionV2 private (sender: PublicKeyAccount, alias: Alias, fee: Long, timestamp: Long, proofs: Proofs)
    extends CreateAliasTransaction {

  override val id: Coeval[AssetId]            = Coeval.evalOnce(ByteStr(crypto.fastHash(builder.typeId +: alias.bytes.arr)))
  override val bodyBytes: Coeval[Array[Byte]] = baseBytes.map(base => Bytes.concat(Array(builder.typeId, version), base))
  override val bytes: Coeval[Array[Byte]]     = (bodyBytes, proofs.bytes).mapN { case (bb, pb) => Bytes.concat(Array(0: Byte), bb, pb) }

  override def builder: TransactionParser = CreateAliasTransactionV2
  override def version: Byte              = 2
}

object CreateAliasTransactionV2 extends TransactionParserFor[CreateAliasTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte = CreateAliasTransaction.typeId

  override def supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(bytes: Array[Byte]): Try[CreateAliasTransactionV2] = {
    Try {
      for {
        (sender, alias, fee, timestamp, end) <- CreateAliasTransaction.parseBase(0, bytes)
        result <- (for {
          proofs <- Proofs.fromBytes(bytes.drop(end))
          tx     <- CreateAliasTransactionV2.create(sender, alias, fee, timestamp, proofs)
        } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
      } yield result
    }.flatten
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
      ConstantByte(1, 0, "Transaction multiple version mark") ~
        ConstantByte(2, value = typeId, name = "Transaction type") ~
        ConstantByte(3, value = 2, name = "Version") ~
        PublicKeyAccountBytes(4, "Sender's public key") ~
        AliasBytes(5, "Alias object") ~
        LongBytes(6, "Fee") ~
        LongBytes(7, "Timestamp") ~
        ProofsBytes(8)
    ).map {
      case (((((((_, _), version), senderPublicKey), alias), fee), timestamp), proofs) =>
        CreateAliasTransactionV2(
          senderPublicKey,
          alias,
          fee,
          timestamp,
          proofs
        )
    }
  }
}
