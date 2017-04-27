package scorex.api.http.assets

import play.api.libs.json.{Format, Json}


case class MakeAssetNameUniqueRequest(sender: String, assetId: String, fee: Long)

object MakeAssetNameUniqueRequest {
  implicit val makeAssetUniqueFormat: Format[MakeAssetNameUniqueRequest] = Json.format
}
