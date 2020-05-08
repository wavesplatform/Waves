package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import play.api.libs.json.Reads._
import play.api.libs.json._

//noinspection TypeAnnotation
private[state] object DiffJsonFormats {
  import com.wavesplatform.utils.ImplicitJsonFormats._

  implicit val volumeAndFee    = Json.format[VolumeAndFee]
  implicit val assetStaticInfo = Json.format[AssetStaticInfo]
  implicit val assetVolumeInfo = Json.format[AssetVolumeInfo]
  implicit val assetScriptInfo = Json.format[AssetScriptInfo]
  implicit val compByEstMapFormat = customMapFormat[Int, Map[String, Long]](_.toString, _.toInt)
    .asInstanceOf[Format[Map[Int, Map[String, Long]]]]
  implicit val accountScriptInfoFormat = Json.format[AccountScriptInfo]
  implicit val assetDescription        = Json.format[AssetDescription]
  implicit val assetInfo               = Json.format[AssetInfo]
  implicit val accountDataInfo         = Json.format[AccountDataInfo]
  implicit val sponsorshipValueFormat  = Json.format[SponsorshipValue]
  implicit val sponsorshipNoInfoFormat = Json.format[SponsorshipNoInfo.type]
  implicit val sponsorshipFormat       = Json.format[Sponsorship]
  implicit val transactionFormat = Format[Transaction](
    implicitly[Reads[ByteStr]].map(tx => PBTransactionSerializer.parseBytes(tx.arr).get),
    Writes(tx => JsString(Base64.encode(PBTransactionSerializer.bytes(tx))))
  )

  implicit val newTransactionInfoFormat = Json.format[NewTransactionInfo]
  implicit val newAssetInfoFormat       = Json.format[NewAssetInfo]
  implicit val transactionsMapFormat    = anyMapFormat[ByteStr, NewTransactionInfo].asInstanceOf[Format[collection.Map[ByteStr, NewTransactionInfo]]]

  implicit val diffFormat = Json.format[Diff]
}
