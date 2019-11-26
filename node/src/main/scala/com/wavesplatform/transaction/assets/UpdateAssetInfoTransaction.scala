package com.wavesplatform.transaction.assets

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.UpdateAssetInfoTxSerializer
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

case class UpdateAssetInfoTransaction(
    version: TxVersion,
    sender: PublicKey,
    asset: IssuedAsset,
    name: Array[Byte],
    description: Array[Byte],
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs
) extends VersionedTransaction
    with ProvenTransaction
    with FastHashId
    with TxWithFee.InWaves
    with LegacyPBSwitch {
  import UpdateAssetInfoTransaction._

  override def builder: TransactionParser = UpdateAssetInfoTransaction

  override val bodyBytes: Coeval[Array[TxType]] = Coeval.evalOnce(serializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxType]]     = Coeval.evalOnce(serializer.toBytes(this))
  override val json: Coeval[JsObject]           = Coeval.evalOnce(serializer.toJson(this))

  override def chainByte: Option[Byte] = Some(AddressScheme.current.chainId)

  override def protobufVersion: TxVersion = TxVersion.V1
}

object UpdateAssetInfoTransaction extends TransactionParser {

  override type TransactionT = UpdateAssetInfoTransaction

  override def typeId: TxType                                 = 17.toByte
  override def supportedVersions: Set[TxVersion]              = Set(TxVersion.V1)
  override def classTag: ClassTag[UpdateAssetInfoTransaction] = ClassTag(classOf[UpdateAssetInfoTransaction])

  private val serializer = UpdateAssetInfoTxSerializer

  override def parseBytes(bytes: Array[Byte]): Try[UpdateAssetInfoTransaction] = serializer.parseBytes(bytes)
}
