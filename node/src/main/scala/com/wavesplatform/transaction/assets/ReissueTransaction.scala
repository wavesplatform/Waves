package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.ReissueTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.ReissueTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util._

case class ReissueTransaction(
    version: TxVersion,
    sender: PublicKey,
    asset: IssuedAsset,
    quantity: Long,
    reissuable: Boolean,
    fee: Long,
    timestamp: Long,
    proofs: Proofs
) extends VersionedTransaction
    with ProvenTransaction
    with SigProofsSwitch
    with TxWithFee.InWaves
    with FastHashId {

  //noinspection TypeAnnotation
  override val builder = ReissueTransaction

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))

  override def chainByte: Option[Byte] = if (version >= TxVersion.V2) Some(AddressScheme.current.chainId) else None

  override def checkedAssets: Seq[IssuedAsset] = Seq(asset)
}

object ReissueTransaction extends TransactionParserLite {
  override type TransactionT = ReissueTransaction

  override val typeId: TxType                         = 5
  override def supportedVersions: Set[TxVersion]      = Set(1, 2)
  override def classTag: ClassTag[ReissueTransaction] = ClassTag(classOf[ReissueTransaction])

  implicit val validator: TxValidator[ReissueTransaction] = ReissueTxValidator
  implicit def sign(tx: ReissueTransaction, privateKey: PrivateKey): ReissueTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  val serializer = ReissueTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[ReissueTransaction] =
    serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] =
    ReissueTransaction(version, sender, asset, quantity, reissuable, fee, timestamp, proofs).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, sender, asset, quantity, reissuable, fee, timestamp, Nil).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, TransactionT] =
    signed(version, sender, asset, quantity, reissuable, fee, timestamp, sender)
}
