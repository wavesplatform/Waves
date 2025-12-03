package com.wavesplatform.state

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import play.api.libs.json.{Format, Json, OWrites}

case class AssetStaticInfo(id: ByteStr, source: TransactionId, issuer: PublicKey, decimals: Int, nft: Boolean)

object AssetStaticInfo {
  implicit val byteStrFormat: Format[ByteStr]   = com.wavesplatform.utils.byteStrFormat
  implicit val format: OWrites[AssetStaticInfo] = Json.writes[AssetStaticInfo]
}
