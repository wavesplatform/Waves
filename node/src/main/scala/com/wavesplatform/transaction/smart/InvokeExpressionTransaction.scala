package com.wavesplatform.transaction.smart

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.InvokeExpressionTxValidator
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class InvokeExpressionTransaction(
    version: TxVersion,
    sender: PublicKey,
    expression: EXPR,
    fee: TxAmount,
    feeAssetId: Asset,
    override val timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends ProvenTransaction
    with VersionedTransaction
    with TxWithFee.InCustomAsset
    with FastHashId
    with LegacyPBSwitch.V2 {

  lazy val expressionBytes  = Serde.serialize(expression, allowObjects = true)
  lazy val expressionBase64 = ByteStr(expressionBytes).base64

  override val builder = InvokeExpressionTransaction

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBTransactionSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(PBTransactionSerializer.bytes(this))

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      BaseTxJson.toJson(this) ++ Json.obj(
        "chainId"    -> this.chainId,
        "expression" -> expressionBase64
      )
    )
}

object InvokeExpressionTransaction extends TransactionParser {
  type TransactionT = InvokeExpressionTransaction

  override val typeId: TxType                    = 18: Byte
  override val supportedVersions: Set[TxVersion] = Set(1)

  implicit def sign(tx: InvokeExpressionTransaction, privateKey: PrivateKey): InvokeExpressionTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  implicit val validator: TxValidator[InvokeExpressionTransaction] = InvokeExpressionTxValidator

  override def parseBytes(bytes: Array[TxType]): Try[InvokeExpressionTransaction] =
    PBTransactionSerializer
      .parseBytes(bytes)
      .flatMap {
        case tx: InvokeExpressionTransaction => Success(tx)
        case tx: Transaction                 => Failure(UnexpectedTransaction(typeId, tx.typeId))
      }

  def create(
      version: Byte,
      sender: PublicKey,
      expression: EXPR,
      feeAmount: TxAmount,
      feeAsset: Asset,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, InvokeExpressionTransaction] =
    InvokeExpressionTransaction(
      version,
      sender,
      expression,
      feeAmount,
      feeAsset,
      timestamp,
      proofs,
      chainId
    ).validatedEither

  def selfSigned(
      version: Byte,
      sender: KeyPair,
      expression: EXPR,
      timestamp: TxTimestamp,
      feeAmount: TxAmount,
      feeAsset: Asset
  ): Either[ValidationError, InvokeExpressionTransaction] =
    create(version, sender.publicKey, expression, feeAmount, feeAsset, timestamp, Proofs.empty)
      .map(_.signWith(sender.privateKey))
}
