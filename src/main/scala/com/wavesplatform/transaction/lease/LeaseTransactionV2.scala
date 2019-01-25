package com.wavesplatform.transaction.lease

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import monix.eval.Coeval
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.ValidationError.UnsupportedVersion
import com.wavesplatform.transaction._

import scala.util.{Either, Failure, Success, Try}

case class LeaseTransactionV2 private (version: Byte,
                                       sender: PublicKeyAccount,
                                       amount: Long,
                                       fee: Long,
                                       timestamp: Long,
                                       recipient: AddressOrAlias,
                                       proofs: Proofs)
    extends LeaseTransaction
    with FastHashId {

  override val builder: TransactionParser = LeaseTransactionV2
  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val assetId: Option[AssetId] = None // placeholder for future enhancement
    Bytes.concat(Array(builder.typeId, version), assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte)), bytesBase())
  }
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object LeaseTransactionV2 extends TransactionParserFor[LeaseTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = LeaseTransaction.typeId
  override def supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val (assetIdOpt, s0) = Deser.parseByteArrayOption(bytes, 0, AssetIdLength)
      (for {
        _      <- Either.cond(assetIdOpt.isEmpty, (), ValidationError.GenericError("Leasing assets is not supported yet"))
        parsed <- LeaseTransaction.parseBase(bytes, s0)
        (sender, recipient, quantity, fee, timestamp, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        lt     <- LeaseTransactionV2.create(version, sender, quantity, fee, timestamp, recipient, proofs)
      } yield lt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), UnsupportedVersion(version))
      _ <- LeaseTransaction.validateLeaseParams(amount, fee, recipient, sender)
    } yield LeaseTransactionV2(version, sender, amount, fee, timestamp, recipient, proofs)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    for {
      unverified <- create(version, sender, amount, fee, timestamp, recipient, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 amount: Long,
                 fee: Long,
                 timestamp: Long,
                 recipient: AddressOrAlias): Either[ValidationError, TransactionT] = {
    signed(version, sender, amount, fee, timestamp, recipient, sender)
  }
}
