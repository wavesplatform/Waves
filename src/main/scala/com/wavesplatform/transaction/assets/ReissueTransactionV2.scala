package com.wavesplatform.transaction.assets

import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import monix.eval.Coeval
import com.wavesplatform.transaction.description._

import scala.util._

case class ReissueTransactionV2 private (chainId: Byte,
                                         sender: PublicKeyAccount,
                                         assetId: ByteStr,
                                         quantity: Long,
                                         reissuable: Boolean,
                                         fee: Long,
                                         timestamp: Long,
                                         proofs: Proofs)
    extends ReissueTransaction
    with FastHashId
    with ChainSpecific {

  override val builder: TransactionParser = ReissueTransactionV2

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        bytesBase(),
      )
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def chainByte: Option[Byte] = Some(chainId)
  override def version: Byte           = 2
}

object ReissueTransactionV2 extends TransactionParserFor[ReissueTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = ReissueTransaction.typeId
  override def supportedVersions: Set[Byte] = Set(2)
  private def currentChainId: Byte          = AddressScheme.current.chainId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val chainId                                                      = bytes(0)
      val (sender, assetId, quantity, reissuable, fee, timestamp, end) = ReissueTransaction.parseBase(bytes, 1)
      (for {
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx <- ReissueTransactionV2
          .create(chainId, sender, assetId, quantity, reissuable, fee, timestamp, proofs)
      } yield tx)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
      _ <- ReissueTransaction.validateReissueParams(quantity, fee)
    } yield ReissueTransactionV2(chainId, sender, assetId, quantity, reissuable, fee, timestamp, proofs)
  }

  def signed(chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    for {
      unverified <- create(chainId, sender, assetId, quantity, reissuable, fee, timestamp, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
  }

  def selfSigned(chainId: Byte,
                 sender: PrivateKeyAccount,
                 assetId: ByteStr,
                 quantity: Long,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(chainId, sender, assetId, quantity, reissuable, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[ReissueTransactionV2] = {
    (
      ConstantByte(1, value = 0, name = "Transaction multiple version mark") ~
        ConstantByte(2, value = typeId, name = "Transaction type") ~
        ConstantByte(3, value = 2, name = "Version") ~
        OneByte(4, "Chain ID") ~
        PublicKeyAccountBytes(5, "Sender's public key") ~
        ByteStrDefinedLength(6, "Asset ID", AssetIdLength) ~
        LongBytes(7, "Quantity") ~
        BooleanByte(8, "Reissuable flag (1 - True, 0 - False)") ~
        LongBytes(9, "Fee") ~
        LongBytes(10, "Timestamp") ~
        ProofsBytes(11)
    ).map {
      case ((((((((((_, _), version), chainId), sender), assetId), quantity), reissuable), fee), timestamp), proofs) =>
        ReissueTransactionV2(
          chainId = chainId,
          sender = sender,
          assetId = assetId,
          quantity = quantity,
          reissuable = reissuable,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
