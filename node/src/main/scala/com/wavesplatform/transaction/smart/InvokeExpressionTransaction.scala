package com.wavesplatform.transaction.smart

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
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
    expression: ExprScript,
    fee: TxAmount,
    feeAssetId: Asset,
    override val timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends InvokeTransaction
    with PBSince.V1 {

  lazy val expressionBytes: ByteStr = expression.bytes.value()

  override def root                = this
  override def senderAddress: Address                         = sender.toAddress
  override val dAppAddressOrAlias: AddressOrAlias             = sender.toAddress
  override val funcCall: Terms.FUNCTION_CALL                  = InvokeTransaction.defaultCall
  override def payments: Seq[InvokeScriptTransaction.Payment] = Nil
  override def checkedAssets: Seq[Asset.IssuedAsset]          = Nil
  override val enableEmptyKeys: Boolean                       = false

  override val builder = InvokeExpressionTransaction

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBTransactionSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(PBTransactionSerializer.bytes(this))
  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      BaseTxJson.toJson(this) ++ Json.obj(
        "chainId"    -> this.chainId,
        "expression" -> expressionBytes.base64
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
      expression: ExprScript,
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
      expression: ExprScript,
      feeAmount: TxAmount,
      feeAsset: Asset,
      timestamp: TxTimestamp
  ): Either[ValidationError, InvokeExpressionTransaction] =
    create(version, sender.publicKey, expression, feeAmount, feeAsset, timestamp, Proofs.empty)
      .map(_.signWith(sender.privateKey))
}
