package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation._
import com.wavesplatform.transaction.validation.impl.UpdateAssetInfoTxValidator
import com.wavesplatform.transaction.{
  Asset,
  ChainSpecific,
  FastHashId,
  Proofs,
  ProvenTransaction,
  Transaction,
  TransactionParser,
  TxAmount,
  TxTimestamp,
  TxType,
  TxVersion,
  UnexpectedTransaction,
  VersionedTransaction,
  _
}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class UpdateAssetInfoTransaction(
    version: TxVersion,
    sender: PublicKey,
    assetId: IssuedAsset,
    name: String,
    description: String,
    feeAssetId: Asset,
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: ChainId
) extends VersionedTransaction
    with ChainSpecific
    with FastHashId
    with ProvenTransaction
    with TxWithFee.InCustomAsset { self =>

  override def assetFee: (Asset, TxAmount) = (feeAssetId, fee)

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
      version: ChainId,
      sender: PublicKey,
      assetId: ByteStr,
      name: String,
      description: String,
      feeAssetId: Asset,
      fee: TxAmount,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: ChainId = ChainId.global
  ): Either[ValidationError, UpdateAssetInfoTransaction] =
    UpdateAssetInfoTransaction(version, sender, IssuedAsset(assetId), name, description, feeAssetId, fee, timestamp, proofs, chainId).validatedEither

  def selfSigned(
      version: ChainId,
      sender: KeyPair,
      assetId: ByteStr,
      name: String,
      description: String,
      feeAssetId: Asset,
      fee: TxAmount,
      timestamp: TxTimestamp
  ): Either[ValidationError, UpdateAssetInfoTransaction] =
    create(version, sender, assetId, name, description, feeAssetId, fee, timestamp, Proofs.empty).map(_.signWith(sender))
}
