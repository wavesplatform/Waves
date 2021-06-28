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
    quantity: TxAmount,
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.Burn) with ProvenTransaction
    with VersionedTransaction
    with SigProofsSwitch
    with TxWithFee.InWaves
    with FastHashId
    with LegacyPBSwitch.V3 {

  override val bodyBytes: Coeval[Array[Byte]] = BurnTxSerializer.bodyBytes(this)
  override val bytes: Coeval[Array[Byte]]     = BurnTxSerializer.toBytes(this)
  override val json: Coeval[JsObject]         = BurnTxSerializer.toJson(this)

  override def checkedAssets: Seq[IssuedAsset] = Seq(asset)
}

object BurnTransaction extends TransactionParser {
  type TransactionT = BurnTransaction

  override val typeId: TxType                    = 6: Byte
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
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, BurnTransaction] =
    BurnTransaction(version, sender, asset, quantity, fee, timestamp, proofs, chainId).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      fee: Long,
      timestamp: Long,
      signer: PrivateKey
  ): Either[ValidationError, BurnTransaction] =
    create(version, sender, asset, quantity, fee, timestamp, Proofs.empty).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      asset: IssuedAsset,
      quantity: Long,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, BurnTransaction] = {
    signed(version, sender.publicKey, asset, quantity, fee, timestamp, sender.privateKey)
  }
}
