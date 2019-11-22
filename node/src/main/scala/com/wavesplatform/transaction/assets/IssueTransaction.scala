package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.serialization.impl.IssueTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.IssueTxValidator
import com.wavesplatform.transaction.{
  FastHashId,
  LegacyPBSwitch,
  Proofs,
  ProvenTransaction,
  SigProofsSwitch,
  TransactionParser,
  TxType,
  TxVersion,
  TxWithFee,
  VersionedTransaction
}
import com.wavesplatform.utils.StrUtils
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

case class IssueTransaction(
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
    proofs: Proofs
) extends VersionedTransaction
    with ProvenTransaction
    with FastHashId
    with SigProofsSwitch
    with TxWithFee.InWaves
    with LegacyPBSwitch.V3 {

  //noinspection TypeAnnotation,ScalaStyle
  override def builder = IssueTransaction

  override val bodyBytes: Coeval[Array[TxType]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxType]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]           = Coeval.evalOnce(builder.serializer.toJson(this))

  override def chainByte: Option[Byte] = if (version == TxVersion.V1) None else Some(AddressScheme.current.chainId)

  private[wavesplatform] lazy val nameBytes = if (isProtobufVersion) StrUtils.toBytesUTF8(name) else StrUtils.toBytesExact(name)
  private[wavesplatform] lazy val descBytes = if (isProtobufVersion) StrUtils.toBytesUTF8(description) else StrUtils.toBytesExact(description)
}

object IssueTransaction extends TransactionParser {
  val MinAssetNameLength        = 4
  val MaxAssetNameLength        = 16
  val MaxAssetDescriptionLength = 1000
  val MaxAssetDecimals          = 8

  override type TransactionT = IssueTransaction

  override val typeId: TxType                       = 3
  override val supportedVersions: Set[TxVersion]    = Set(1, 2, 3)
  override def classTag: ClassTag[IssueTransaction] = ClassTag(classOf[IssueTransaction])

  val serializer = IssueTxSerializer

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
      proofs: Proofs
  ): Either[ValidationError, TransactionT] =
    IssueTransaction(version, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs).validatedEither

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
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, Proofs.empty).map(_.signWith(signer))

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
      timestamp: Long
  ): Either[ValidationError, TransactionT] =
    signed(version, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, sender)

  override def parseBytes(bytes: Array[TxType]): Try[IssueTransaction] = serializer.parseBytes(bytes)

  implicit class IssueTransactionExt(private val tx: IssueTransaction) extends AnyVal {
    def assetId: ByteStr = tx.id()
    def isNFT: Boolean   = tx.quantity == 1 && tx.decimals == 0 && !tx.reissuable
    def isNFT(blockchain: Blockchain): Boolean = {
      import com.wavesplatform.features.BlockchainFeatures
      import com.wavesplatform.features.FeatureProvider._
      blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee) && this.isNFT
    }
  }
}
