package com.wavesplatform.transaction.assets

import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.IssueTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.IssueTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class IssueTransaction(
    version: TxVersion,
    sender: PublicKey,
    name: ByteString,
    description: ByteString,
    quantity: TxPositiveAmount,
    decimals: TxDecimals,
    reissuable: Boolean,
    script: Option[Script],
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.Issue)
    with VersionedTransaction.ToV3
    with ProvenTransaction
    with FastHashId
    with SigProofsSwitch
    with TxWithFee.InWaves
    with PBSince.V3 {

  override val bodyBytes: Coeval[Array[TxType]] = Coeval.evalOnce(IssueTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxType]]     = Coeval.evalOnce(IssueTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]           = Coeval.evalOnce(IssueTxSerializer.toJson(this))
}

object IssueTransaction extends TransactionParser {
  type TransactionT = IssueTransaction

  val MinAssetNameLength        = 4
  val MaxAssetNameLength        = 16
  val MaxAssetDescriptionLength = 1000
  val MaxAssetDecimals          = 8

  override val typeId: TxType                    = 3: Byte
  override val supportedVersions: Set[TxVersion] = Set(1, 2, 3)

  implicit val validator: TxValidator[IssueTransaction] = IssueTxValidator
  implicit def sign(tx: IssueTransaction, privateKey: PrivateKey): IssueTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  def create(
      version: TxVersion,
      sender: PublicKey,
      name: String,
      description: String,
      quantity: Long,
      decimals: Byte,
      reissuable: Boolean,
      script: Option[Script],
      fee: Long,
      timestamp: Long,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, IssueTransaction] =
    for {
      fee      <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      quantity <- TxPositiveAmount(quantity)(TxValidationError.NonPositiveAmount(quantity, "assets"))
      decimals <- TxDecimals(decimals)(TxValidationError.InvalidDecimals(decimals))
      tx <- IssueTransaction(
        version,
        sender,
        ByteString.copyFromUtf8(name),
        ByteString.copyFromUtf8(description),
        quantity,
        decimals,
        reissuable,
        script,
        fee,
        timestamp,
        proofs,
        chainId
      ).validatedEither
    } yield tx

  def signed(
      version: TxVersion,
      sender: PublicKey,
      name: String,
      description: String,
      quantity: Long,
      decimals: Byte,
      reissuable: Boolean,
      script: Option[Script],
      fee: Long,
      timestamp: Long,
      signer: PrivateKey,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, IssueTransaction] =
    create(version, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, Proofs.empty, chainId).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      name: String,
      description: String,
      quantity: Long,
      decimals: Byte,
      reissuable: Boolean,
      script: Option[Script],
      fee: Long,
      timestamp: Long,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, IssueTransaction] =
    signed(version, sender.publicKey, name, description, quantity, decimals, reissuable, script, fee, timestamp, sender.privateKey, chainId)

  override def parseBytes(bytes: Array[TxType]): Try[IssueTransaction] = IssueTxSerializer.parseBytes(bytes)

  implicit class IssueTransactionExt(private val tx: IssueTransaction) extends AnyVal {
    def asset: IssuedAsset = IssuedAsset(assetId)
    def assetId: ByteStr   = tx.id()
  }
}
