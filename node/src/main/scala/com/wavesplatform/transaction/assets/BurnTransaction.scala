package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
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
    quantity: Long,
    fee: Long,
    timestamp: Long,
    proofs: Proofs
) extends ProvenTransaction
    with VersionedTransaction
    with SigProofsSwitch
    with TxWithFee.InWaves
    with FastHashId
    with LegacyPBSwitch.V3 {

  //noinspection TypeAnnotation,ScalaStyle
  override def builder = BurnTransaction

  override def chainByte: Option[Byte] = if (version >= TxVersion.V2) Some(AddressScheme.current.chainId) else None

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))

  override def checkedAssets: Seq[IssuedAsset] = Seq(asset)
}

object BurnTransaction extends TransactionParser {
  override type TransactionT = BurnTransaction

  override val typeId: TxType                    = 6
  override val supportedVersions: Set[TxVersion] = Set(1, 2, 3)

  implicit val validator: TxValidator[BurnTransaction] = BurnTxValidator

  implicit def sign(tx: BurnTransaction, privateKey: PrivateKey): BurnTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

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
      proofs: Proofs
  ): Either[ValidationError, BurnTransaction] =
    BurnTransaction(version, sender, asset, quantity, fee, timestamp, proofs).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      fee: Long,
      timestamp: Long,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, sender, asset, quantity, fee, timestamp, Proofs.empty).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      asset: IssuedAsset,
      quantity: Long,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, TransactionT] = {
    signed(version, sender, asset, quantity, fee, timestamp, sender)
  }
}
