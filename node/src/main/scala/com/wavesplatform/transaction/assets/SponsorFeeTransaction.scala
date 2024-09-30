package com.wavesplatform.transaction.assets

import cats.syntax.traverse.*
import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.NegativeMinFee
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.serialization.impl.SponsorFeeTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.SponsorFeeTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class SponsorFeeTransaction(
    version: TxVersion,
    sender: PublicKey,
    asset: IssuedAsset,
    minSponsoredAssetFee: Option[TxPositiveAmount],
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.SponsorFee, Seq(asset))
    with ProvenTransaction
    with Versioned.ToV2
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V2 {

  val bodyBytes: Coeval[Array[Byte]]      = Coeval.evalOnce(SponsorFeeTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(SponsorFeeTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]     = Coeval.evalOnce(SponsorFeeTxSerializer.toJson(this))
}

object SponsorFeeTransaction extends TransactionParser {
  type TransactionT = SponsorFeeTransaction
  override val typeId: TxType = 14: Byte

  implicit val validator: TxValidator[SponsorFeeTransaction] = SponsorFeeTxValidator

  implicit def sign(tx: SponsorFeeTransaction, privateKey: PrivateKey): SponsorFeeTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[SponsorFeeTransaction] =
    SponsorFeeTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      minSponsoredAssetFee: Option[Long],
      fee: Long,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, SponsorFeeTransaction] =
    for {
      fee                  <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      minSponsoredAssetFee <- minSponsoredAssetFee.traverse(fee => TxPositiveAmount(fee)(NegativeMinFee(fee, "asset")))
      tx                   <- SponsorFeeTransaction(version, sender, asset, minSponsoredAssetFee, fee, timestamp, proofs, chainId).validatedEither
    } yield tx

  def signed(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      minSponsoredAssetFee: Option[Long],
      fee: Long,
      timestamp: TxTimestamp,
      signer: PrivateKey,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, SponsorFeeTransaction] =
    create(version, sender, asset, minSponsoredAssetFee, fee, timestamp, Proofs.empty, chainId).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      asset: IssuedAsset,
      minSponsoredAssetFee: Option[Long],
      fee: Long,
      timestamp: TxTimestamp,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, SponsorFeeTransaction] =
    signed(version, sender.publicKey, asset, minSponsoredAssetFee, fee, timestamp, sender.privateKey, chainId)
}
