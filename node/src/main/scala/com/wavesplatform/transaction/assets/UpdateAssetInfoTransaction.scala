package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation.*
import com.wavesplatform.transaction.validation.impl.UpdateAssetInfoTxValidator
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class UpdateAssetInfoTransaction(
    override val version: TxVersion,
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
    with Versioned.ConstV1
    with FastHashId
    with ProvenTransaction
    with PBSince.V1 { self =>
  override type T = UpdateAssetInfoTransaction
  override def addProof(proof: ByteStr): UpdateAssetInfoTransaction = copy(proofs = this.proofs.add(proof))

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

object UpdateAssetInfoTransaction extends TransactionParser {
  type TransactionT = UpdateAssetInfoTransaction
  override val typeId: TxType = 17: Byte

  implicit val validator: TxValidator[UpdateAssetInfoTransaction] = UpdateAssetInfoTxValidator

  def create(
      version: Byte,
      sender: PublicKey,
      assetId: IssuedAsset,
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
        assetId,
        name,
        description,
        timestamp,
        fee,
        feeAsset,
        proofs,
        chainId
      ).validatedEither
    } yield tx

  override def parseBytes(bytes: Array[Byte]): Try[UpdateAssetInfoTransaction] =
    PBTransactionSerializer
      .parseBytes(bytes)
      .flatMap {
        case tx: UpdateAssetInfoTransaction => Success(tx)
        case tx: Transaction                => Failure(UnexpectedTransaction(typeId, tx.tpe.id.toByte))
      }
}
