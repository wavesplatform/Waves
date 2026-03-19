package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.serialization.impl.BurnTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.BurnTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

final case class BurnTransaction(
    version: TxVersion,
    sender: PublicKey,
    asset: IssuedAsset,
    quantity: TxNonNegativeAmount,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.Burn, Seq(asset))
    with ProvenTransaction
    with Versioned.ToV3
    with HasSignature
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V3 {

  type T = BurnTransaction

  override val bodyBytes: Coeval[Array[Byte]] = BurnTxSerializer.bodyBytes(this)
  override val bytes: Coeval[Array[Byte]]     = BurnTxSerializer.toBytes(this)
  override val json: Coeval[JsObject]         = BurnTxSerializer.toJson(this)

  override def addProof(proof: ByteStr): BurnTransaction = copy(proofs = this.proofs.add(proof))
}

object BurnTransaction extends TransactionParser {
  type TransactionT = BurnTransaction
  override val typeId: TxType = 6: Byte

  implicit val validator: TxValidator[BurnTransaction] = BurnTxValidator

  val serializer = BurnTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[BurnTransaction] =
    serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      fee: Long,
      timestamp: Long,
      proofs: Proofs = Proofs.empty,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, BurnTransaction] =
    for {
      quantity <- TxNonNegativeAmount(quantity)(TxValidationError.NegativeAmount(quantity, "assets"))
      fee      <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx       <- BurnTransaction(version, sender, asset, quantity, fee, timestamp, proofs, chainId).validatedEither
    } yield tx
}
