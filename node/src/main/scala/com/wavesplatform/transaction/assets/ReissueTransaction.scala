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

import scala.util._

case class ReissueTransaction(
                               version: TxVersion,
                               sender: PublicKey,
                               asset: IssuedAsset,
                               quantity: TxPositiveAmount,
                               reissuable: Boolean,
                               fee: TxPositiveAmount,
                               timestamp: TxTimestamp,
                               proofs: Proofs,
                               chainId: Byte
) extends VersionedTransaction
    with ProvenTransaction
    with SigProofsSwitch
    with TxWithFee.InWaves
    with FastHashId
    with LegacyPBSwitch.V3 {

  //noinspection TypeAnnotation
  override val builder = ReissueTransaction

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))

  override def checkedAssets: Seq[IssuedAsset] = Seq(asset)
}

object ReissueTransaction extends TransactionParser {
  type TransactionT = ReissueTransaction

  override val typeId: TxType                    = 5: Byte
  override def supportedVersions: Set[TxVersion] = Set(1, 2, 3)

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
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, ReissueTransaction] =
    for {
      fee <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      quantity <- TxPositiveAmount(quantity)(TxValidationError.NonPositiveAmount(quantity, "assets"))
      tx <-ReissueTransaction(version, sender, asset, quantity, reissuable, fee, timestamp, proofs, chainId).validatedEither
    } yield tx

  def signed(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long,
      signer: PrivateKey,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, ReissueTransaction] =
    create(version, sender, asset, quantity, reissuable, fee, timestamp, Nil,chainId).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, ReissueTransaction] =
    signed(version, sender.publicKey, asset, quantity, reissuable, fee, timestamp, sender.privateKey, chainId)
}
