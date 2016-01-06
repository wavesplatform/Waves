package scorex.perma.settings

import play.api.libs.json.JsObject
import scorex.crypto.encode.Base58

trait PermaSettings {
  val settingsJSON: JsObject

  lazy val rootHash: Array[Byte] = Base58.decode("FQb7JGmZayjS9Y9qMpRtWZtW8BXrGRCbbuX2YNH5Q54t").get

  lazy val isTrustedDealer = (settingsJSON \ "perma" \ "isTrustedDealer").asOpt[Boolean].getOrElse(false)

  lazy val treeDir = (settingsJSON \ "perma" \ "treeDir").as[String]

  lazy val authDataStorage = treeDir + (settingsJSON \ "perma" \ "authDataStorage").as[String]
}
