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

import scala.util.{Failure, Success, Try}

case class UpdateAssetInfoTransaction(
    version: TxVersion,
    sender: PublicKey,
    assetId: IssuedAsset,
    name: String,
    description: String,
    timestamp: TxTimestamp,
    feeAmount: TxAmount,
    feeAsset: Asset,
    proofs: Proofs,
    chainId: Byte
) extends VersionedTransaction
    with FastHashId
    with ProvenTransaction
    with ProtobufOnly { self =>

  override def assetFee: (Asset, TxAmount) = (feeAsset, feeAmount)

  override def builder: UpdateAssetInfoTransaction.type = UpdateAssetInfoTransaction

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

  override def checkedAssets: Seq[IssuedAsset] = Seq(assetId)
}

object UpdateAssetInfoTransaction extends TransactionParser {
  type TransactionT = UpdateAssetInfoTransaction

  override val typeId: TxType                    = 17: Byte
  override val supportedVersions: Set[TxVersion] = Set(1)

  implicit def sign(tx: UpdateAssetInfoTransaction, privateKey: PrivateKey): UpdateAssetInfoTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  implicit val validator: TxValidator[UpdateAssetInfoTransaction] = UpdateAssetInfoTxValidator

  override def parseBytes(bytes: Array[TxType]): Try[UpdateAssetInfoTransaction] =
    PBTransactionSerializer
      .parseBytes(bytes)
      .flatMap {
        case tx: UpdateAssetInfoTransaction => Success(tx)
        case tx: Transaction                => Failure(UnexpectedTransaction(typeId, tx.typeId))
      }

  def create(
      version: Byte,
      sender: PublicKey,
      assetId: ByteStr,
      name: String,
      description: String,
      timestamp: TxTimestamp,
      feeAmount: TxAmount,
      feeAsset: Asset,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, UpdateAssetInfoTransaction] = {
    UpdateAssetInfoTransaction(
      version,
      sender,
      IssuedAsset(assetId),
      name,
      description,
      timestamp,
      feeAmount,
      feeAsset,
      proofs,
      chainId
    ).validatedEither
  }

  def selfSigned(
      version: Byte,
      sender: KeyPair,
      assetId: ByteStr,
      name: String,
      description: String,
      timestamp: TxTimestamp,
      feeAmount: TxAmount,
      feeAsset: Asset
  ): Either[ValidationError, UpdateAssetInfoTransaction] =
    create(version, sender.publicKey, assetId, name, description, timestamp, feeAmount, feeAsset, Proofs.empty).map(_.signWith(sender.privateKey))
}
