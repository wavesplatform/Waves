package com.wavesplatform.transaction.smart

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.InvokeExpressionTxValidator
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class InvokeExpressionTransaction(
    override val version: TxVersion,
    sender: PublicKey,
    expression: ExprScript,
    fee: TxPositiveAmount,
    feeAssetId: Asset,
    override val timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.InvokeExpression, Nil)
    with InvokeTransaction
    with Versioned.ConstV1
    with PBSince.V1 {

  lazy val expressionBytes: ByteStr = expression.bytes.value()

  override def dApp: AddressOrAlias                           = sender.toAddress
  override def root: InvokeExpressionTransaction              = this
  override val funcCall: Terms.FUNCTION_CALL                  = InvokeTransaction.DefaultCall
  override def payments: Seq[InvokeScriptTransaction.Payment] = Nil
  override val checkedAssets: Seq[Asset.IssuedAsset]          = Nil

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

  override val typeId: TxType = 18: Byte

  implicit def sign(tx: InvokeExpressionTransaction, privateKey: PrivateKey): InvokeExpressionTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  implicit val validator: TxValidator[InvokeExpressionTransaction] = InvokeExpressionTxValidator

  override def parseBytes(bytes: Array[TxType]): Try[InvokeExpressionTransaction] =
    PBTransactionSerializer
      .parseBytes(bytes)
      .flatMap {
        case tx: InvokeExpressionTransaction => Success(tx)
        case tx: Transaction                 => Failure(UnexpectedTransaction(typeId, tx.tpe.id.toByte))
      }

  def create(
      version: Byte,
      sender: PublicKey,
      expression: ExprScript,
      feeAmount: Long,
      feeAsset: Asset,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, InvokeExpressionTransaction] =
    for {
      fee <- TxPositiveAmount(feeAmount)(TxValidationError.InsufficientFee)
      tx <- InvokeExpressionTransaction(
        version,
        sender,
        expression,
        fee,
        feeAsset,
        timestamp,
        proofs,
        chainId
      ).validatedEither
    } yield tx

  def selfSigned(
      version: Byte,
      sender: KeyPair,
      expression: ExprScript,
      feeAmount: Long,
      feeAsset: Asset,
      timestamp: TxTimestamp
  ): Either[ValidationError, InvokeExpressionTransaction] =
    create(version, sender.publicKey, expression, feeAmount, feeAsset, timestamp, Proofs.empty)
      .map(_.signWith(sender.privateKey))
}
