package com.wavesplatform.transaction.smart

import com.wavesplatform.account._
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.transaction.Asset._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.InvokeScriptTxSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.InvokeScriptTxValidator
import com.wavesplatform.state.diffs.invoke.InvokeScriptLike
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class InvokeScriptTransaction(
    version: TxVersion,
    sender: PublicKey,
    dAppAddressOrAlias: AddressOrAlias,
    funcCallOpt: Option[FUNCTION_CALL],
    payments: Seq[Payment],
    fee: TxAmount,
    feeAssetId: Asset,
    override val timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends ProvenTransaction
    with VersionedTransaction
    with TxWithFee.InCustomAsset
    with FastHashId
    with LegacyPBSwitch.V2
    with InvokeScriptLike {

  val funcCall = funcCallOpt.getOrElse(FUNCTION_CALL(FunctionHeader.User(ContractEvaluator.DEFAULT_FUNC_NAME), List.empty))

  override val builder = InvokeScriptTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))

  override def root: Option[InvokeScriptTransaction] = Some(this)
  def senderAddress: Address = sender.toAddress
  override def checkedAssets: Seq[IssuedAsset] = super[InvokeScriptLike].checkedAssets
}

object InvokeScriptTransaction extends TransactionParser {
  type TransactionT = InvokeScriptTransaction

  override val typeId: TxType                    = 16: Byte
  override val supportedVersions: Set[TxVersion] = Set(1, 2)

  implicit val validator: TxValidator[InvokeScriptTransaction] = InvokeScriptTxValidator

  implicit def sign(tx: InvokeScriptTransaction, privateKey: PrivateKey): InvokeScriptTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  val serializer = InvokeScriptTxSerializer

  override def parseBytes(bytes: Array[Byte]): Try[InvokeScriptTransaction] =
    serializer.parseBytes(bytes)

  case class Payment(amount: TxAmount, assetId: Asset)
  object Payment {
    import play.api.libs.json.{Json, _}
    implicit val jsonFormat: Format[Payment] = Json.format
  }

  def create(
      version: TxVersion,
      sender: PublicKey,
      dappAddress: AddressOrAlias,
      fc: Option[FUNCTION_CALL],
      p: Seq[Payment],
      fee: TxAmount,
      feeAssetId: Asset,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, InvokeScriptTransaction] =
    InvokeScriptTransaction(version, sender, dappAddress, fc, p, fee, feeAssetId, timestamp, proofs, dappAddress.chainId).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      dappAddress: AddressOrAlias,
      fc: Option[FUNCTION_CALL],
      p: Seq[Payment],
      fee: TxAmount,
      feeAssetId: Asset,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, InvokeScriptTransaction] =
    create(version, sender, dappAddress, fc, p, fee, feeAssetId, timestamp, Proofs.empty).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      dappAddress: AddressOrAlias,
      fc: Option[FUNCTION_CALL],
      p: Seq[Payment],
      fee: TxAmount,
      feeAssetId: Asset,
      timestamp: TxTimestamp
  ): Either[ValidationError, InvokeScriptTransaction] =
    signed(version, sender.publicKey, dappAddress, fc, p, fee, feeAssetId, timestamp, sender.privateKey)
}
