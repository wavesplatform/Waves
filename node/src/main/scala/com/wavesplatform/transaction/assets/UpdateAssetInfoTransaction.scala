package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation._
import com.wavesplatform.transaction.validation.impl.UpdateAssetInfoTxValidator
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

case class UpdateAssetInfoTransaction(
    version: TxVersion,
    sender: PublicKey,
    assetId: IssuedAsset,
    name: String,
    description: String,
    timestamp: TxTimestamp,
    feeAmount: TxPositiveAmount,
    feeAsset: Asset,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.UpdateAssetInfo, Seq(assetId))
    with VersionedTransaction
    with FastHashId
    with ProvenTransaction
    with PBSince.V1 { self =>

  override def assetFee: (Asset, Long) = (feeAsset, feeAmount.value)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBTransactionSerializer.bodyBytes(self))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(PBTransactionSerializer.bytes(self))

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      BaseTxJson.toJson(self) ++ Json.obj(
        "chainId"     -> self.chainId,
        "assetId"     -> (self.assetId: Asset),
        "name"        -> self.name,
        "description" -> self.description
      )
    )
}

object UpdateAssetInfoTransaction {
  val supportedVersions: Set[TxVersion] = Set(1)

  implicit def sign(tx: UpdateAssetInfoTransaction, privateKey: PrivateKey): UpdateAssetInfoTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  implicit val validator: TxValidator[UpdateAssetInfoTransaction] = UpdateAssetInfoTxValidator

  def create(
      version: Byte,
      sender: PublicKey,
      assetId: ByteStr,
      name: String,
      description: String,
      timestamp: TxTimestamp,
      feeAmount: Long,
      feeAsset: Asset,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, UpdateAssetInfoTransaction] =
    for {
      fee <- TxPositiveAmount(feeAmount)(TxValidationError.InsufficientFee)
      tx <- UpdateAssetInfoTransaction(
        version,
        sender,
        IssuedAsset(assetId),
        name,
        description,
        timestamp,
        fee,
        feeAsset,
        proofs,
        chainId
      ).validatedEither
    } yield tx

  def selfSigned(
      version: Byte,
      sender: KeyPair,
      assetId: ByteStr,
      name: String,
      description: String,
      timestamp: TxTimestamp,
      feeAmount: Long,
      feeAsset: Asset,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, UpdateAssetInfoTransaction] =
    create(version, sender.publicKey, assetId, name, description, timestamp, feeAmount, feeAsset, Proofs.empty, chainId)
      .map(_.signWith(sender.privateKey))
}
