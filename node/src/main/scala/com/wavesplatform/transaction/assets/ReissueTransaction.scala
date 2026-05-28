package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.serialization.impl.ReissueTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.ReissueTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.*

case class ReissueTransaction(
    version: TxVersion,
    sender: PublicKey,
    asset: IssuedAsset,
    quantity: TxPositiveAmount,
    reissuable: Boolean,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.Reissue, Seq(asset))
    with Versioned.ToV3
    with ProvenTransaction
    with HasSignature
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V3 {
  override type T = ReissueTransaction

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(ReissueTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(ReissueTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(ReissueTxSerializer.toJson(this))

  override def addProof(proof: ByteStr): ReissueTransaction = copy(proofs = this.proofs.add(proof))
}

object ReissueTransaction extends TransactionParser {
  type TransactionT = ReissueTransaction

  override val typeId: TxType = 5: Byte

  implicit val validator: TxValidator[ReissueTransaction] = ReissueTxValidator
  implicit def sign(tx: ReissueTransaction, privateKey: PrivateKey): ReissueTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[ReissueTransaction] =
    ReissueTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long,
      proofs: Proofs = Proofs.empty,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, ReissueTransaction] =
    for {
      fee      <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      quantity <- TxPositiveAmount(quantity)(TxValidationError.NonPositiveAmount(quantity, "assets"))
      tx       <- ReissueTransaction(version, sender, asset, quantity, reissuable, fee, timestamp, proofs, chainId).validatedEither
    } yield tx
}
