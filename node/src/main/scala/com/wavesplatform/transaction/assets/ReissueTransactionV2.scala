package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util._

case class ReissueTransactionV2 private (chainId: Byte,
                                         sender: PublicKey,
                                         asset: IssuedAsset,
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
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${tx.chainId.toInt}, expected: $currentChainId"))
        .flatMap(_ => ReissueTransaction.validateReissueParams(tx))
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(chainId: Byte,
             sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
      _ <- ReissueTransaction.validateReissueParams(quantity, fee)
    } yield ReissueTransactionV2(chainId, sender, asset, quantity, reissuable, fee, timestamp, proofs)
  }

  def signed(chainId: Byte,
             sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    for {
      unverified <- create(chainId, sender, asset, quantity, reissuable, fee, timestamp, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
  }

  def selfSigned(chainId: Byte,
                 sender: KeyPair,
                 asset: IssuedAsset,
                 quantity: Long,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(chainId, sender, asset, quantity, reissuable, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[ReissueTransactionV2] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyBytes(tailIndex(2), "Sender's public key"),
      ByteStrDefinedLength(tailIndex(3), "Asset ID", AssetIdLength).map(IssuedAsset),
      LongBytes(tailIndex(4), "Quantity"),
      BooleanByte(tailIndex(5), "Reissuable flag (1 - True, 0 - False)"),
      LongBytes(tailIndex(6), "Fee"),
      LongBytes(tailIndex(7), "Timestamp"),
      ProofsBytes(tailIndex(8))
    ) mapN ReissueTransactionV2.apply
  }
}
