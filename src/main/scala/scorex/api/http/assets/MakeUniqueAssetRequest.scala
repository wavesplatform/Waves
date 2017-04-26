package scorex.api.http.assets

import play.api.libs.json.{Format, Json}


case class MakeUniqueAssetRequest(sender: String, assetId: String, fee: Long)

object MakeUniqueAssetRequest {
  implicit val makeAssetUniqueFormat: Format[MakeUniqueAssetRequest] = Json.format
}
