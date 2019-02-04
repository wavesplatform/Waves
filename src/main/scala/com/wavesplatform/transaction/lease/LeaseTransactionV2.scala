package com.wavesplatform.transaction.lease

import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.{Either, Failure, Success, Try}

case class LeaseTransactionV2 private (sender: PublicKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias, proofs: Proofs)
    extends LeaseTransaction
    with FastHashId {

  override val builder: TransactionParser = LeaseTransactionV2

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val assetId: Option[AssetId] = None // placeholder for future enhancement
    Bytes.concat(Array(builder.typeId, version), assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte)), bytesBase())
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 2
}

object LeaseTransactionV2 extends TransactionParserFor[LeaseTransactionV2] with TransactionParser.MultipleVersions {

  override def supportedVersions: Set[Byte] = Set(2)

  override val typeId: Byte = LeaseTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val (assetIdOpt, s0) = Deser.parseByteArrayOption(bytes, 0, AssetIdLength)
      (for {
        _      <- Either.cond(assetIdOpt.isEmpty, (), ValidationError.GenericError("Leasing assets is not supported yet"))
        parsed <- LeaseTransaction.parseBase(bytes, s0)
        (sender, recipient, quantity, fee, timestamp, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        lt     <- LeaseTransactionV2.create(sender, quantity, fee, timestamp, recipient, proofs)
      } yield lt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- LeaseTransaction.validateLeaseParams(amount, fee, recipient, sender)
    } yield LeaseTransactionV2(sender, amount, fee, timestamp, recipient, proofs)
  }

  def signed(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    for {
      unverified <- create(sender, amount, fee, timestamp, recipient, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
  }

  def selfSigned(sender: PrivateKeyAccount,
                 amount: Long,
                 fee: Long,
                 timestamp: Long,
                 recipient: AddressOrAlias): Either[ValidationError, TransactionT] = {
    signed(sender, amount, fee, timestamp, recipient, sender)
  }

  val byteTailDescription: ByteEntity[LeaseTransactionV2] = {
    (
      ConstantByte(1, value = 0, name = "Transaction multiple version mark") ~
        ConstantByte(2, value = typeId, name = "Transaction type") ~
        ConstantByte(3, value = 2, name = "Version") ~
        OptionAssetIdBytes(4, "Leasing asset (Only Waves are currently supported)") ~
        PublicKeyAccountBytes(5, "Sender's public key") ~
        AddressOrAliasBytes(6, "Recipient") ~
        LongBytes(7, "Amount") ~
        LongBytes(8, "Fee") ~
        LongBytes(9, "Timestamp") ~
        ProofsBytes(10)
    ).map {
      case (((((((((_, _), version), _), senderPublicKey), recipient), amount), fee), timestamp), proofs) =>
        LeaseTransactionV2(
          sender = senderPublicKey,
          amount = amount,
          fee = fee,
          timestamp = timestamp,
          recipient = recipient,
          proofs = proofs
        )
    }
  }
}
