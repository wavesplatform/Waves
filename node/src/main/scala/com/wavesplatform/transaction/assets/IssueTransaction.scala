package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64.encode
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.IssueTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.IssueTxValidator
import com.wavesplatform.utils._
import monix.eval.Coeval
import play.api.libs.json.JsObject

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

  def safeName: Either[ByteStr, String] = if (isProtobufVersion) Right(name) else Left(this.nameBytes)
  def safeDescription: Either[ByteStr, String] = if (isProtobufVersion) Right(description) else Left(this.descriptionBytes)
}

object IssueTransaction extends TransactionParser {
  val MinAssetNameLength        = 4
  val MaxAssetNameLength        = 16
  val MaxAssetDescriptionLength = 1000
  val MaxAssetDecimals          = 8

  override val typeId: TxType                    = 3
  override val supportedVersions: Set[TxVersion] = Set(1, 2, 3)

  val serializer = IssueTxSerializer

  implicit val validator: TxValidator[IssueTransaction] = IssueTxValidator
  implicit def sign(tx: IssueTransaction, privateKey: PrivateKey): IssueTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  def apply(
      version: TxVersion,
      sender: PublicKey,
      nameBytes: Array[Byte],
      descriptionBytes: Array[Byte],
      quantity: Long,
      decimals: Byte,
      reissuable: Boolean,
      script: Option[Script],
      fee: Long,
      timestamp: Long,
      proofs: Proofs = Proofs.empty
  ): IssueTransaction = {
    require(version <= 2, "bytes in name and description are only supported in versions <= 3")
    IssueTransaction(version, sender, encode(nameBytes), encode(descriptionBytes), quantity, decimals, reissuable, script, fee, timestamp, proofs)
  }

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
  ): Either[ValidationError, IssueTransaction] =
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
  ): Either[ValidationError, IssueTransaction] =
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
  ): Either[ValidationError, IssueTransaction] =
    signed(version, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, sender)

  override def parseBytes(bytes: Array[TxType]): Try[IssueTransaction] = serializer.parseBytes(bytes)

  private def decode(tx: IssueTransaction)(v: IssueTransaction => String): ByteStr =
    if (tx.isProtobufVersion) ByteStr(v(tx).utf8Bytes) else ByteStr.decodeBase64(v(tx)).get

  implicit class IssueTransactionExt(private val tx: IssueTransaction) extends AnyVal {
    def assetId: ByteStr = tx.id()
    def isNFT: Boolean   = tx.quantity == 1 && tx.decimals == 0 && !tx.reissuable
    def isNFT(blockchain: Blockchain): Boolean = {
      import com.wavesplatform.features.BlockchainFeatures
      import com.wavesplatform.features.FeatureProvider._
      blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee) && this.isNFT
    }
    def nameBytes: ByteStr        = decode(tx)(_.name)
    def descriptionBytes: ByteStr = decode(tx)(_.description)
  }
}
