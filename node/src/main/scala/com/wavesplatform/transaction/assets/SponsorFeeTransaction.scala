package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
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
    minSponsoredAssetFee: Option[TxAmount],
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends ProvenTransaction
    with VersionedTransaction
    with TxWithFee.InWaves
    with FastHashId
    with LegacyPBSwitch.V2 {

  override val builder: SponsorFeeTransaction.type = SponsorFeeTransaction

  val bodyBytes: Coeval[Array[Byte]]      = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]     = Coeval.evalOnce(builder.serializer.toJson(this))

  override val checkedAssets: Seq[IssuedAsset] = Seq(asset)
}

object SponsorFeeTransaction extends TransactionParser {
  type TransactionT = SponsorFeeTransaction

  override val typeId: TxType                    = 14: Byte
  override val supportedVersions: Set[TxVersion] = Set(1, 2)

  implicit val validator: TxValidator[SponsorFeeTransaction] = SponsorFeeTxValidator

  implicit def sign(tx: SponsorFeeTransaction, privateKey: PrivateKey): SponsorFeeTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  val serializer = SponsorFeeTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[SponsorFeeTransaction] =
    serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      minSponsoredAssetFee: Option[TxTimestamp],
      fee: TxAmount,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, SponsorFeeTransaction] =
    SponsorFeeTransaction(version, sender, asset, minSponsoredAssetFee, fee, timestamp, proofs, chainId).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      minSponsoredAssetFee: Option[TxTimestamp],
      fee: TxAmount,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, SponsorFeeTransaction] =
    create(version, sender, asset, minSponsoredAssetFee, fee, timestamp, Proofs.empty).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      asset: IssuedAsset,
      minSponsoredAssetFee: Option[TxTimestamp],
      fee: TxAmount,
      timestamp: TxTimestamp
  ): Either[ValidationError, SponsorFeeTransaction] =
    signed(version, sender.publicKey, asset, minSponsoredAssetFee, fee, timestamp, sender.privateKey)
}
