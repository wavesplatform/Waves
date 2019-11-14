package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
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
    with TxWithFee.InWaves
    with FastHashId {

  override val builder = ReissueTransaction

  override val bodyBytes: Coeval[Array[Byte]] = ???
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(???)
  override val json: Coeval[JsObject]         = ???

  override def chainByte: Option[Byte] = if (version >= TxVersion.V2) Some(AddressScheme.current.chainId) else None
}

object ReissueTransaction extends TransactionParserLite {
  override type TransactionT = ReissueTransaction

  override val typeId: TxType                         = ReissueTransaction.typeId
  override def supportedVersions: Set[TxVersion]      = Set(1, 2)
  override def classTag: ClassTag[ReissueTransaction] = ClassTag(classOf[ReissueTransaction])

  override def parseBytes(bytes: Array[TxVersion]): Try[ReissueTransaction] = ???

  def create(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] = ???

  def signed(
      version: TxVersion,
      sender: PublicKey,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] = ???

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      asset: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, TransactionT] = ???
}
