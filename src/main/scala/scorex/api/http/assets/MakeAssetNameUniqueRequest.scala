package scorex.api.http.assets

import play.api.libs.json.{Format, Json}


case class MakeAssetNameUniqueRequest(sender: String, assetId: String, fee: Long, networkByte: Byte)

object MakeAssetNameUniqueRequest {
  implicit val makeAssetUniqueFormat: Format[MakeAssetNameUniqueRequest] = Json.format
}
