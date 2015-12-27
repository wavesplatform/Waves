package scorex.perma.settings

import play.api.libs.json.JsObject

trait PermaSettings {
  val settingsJSON: JsObject

  private val DefaultTreeDir = "/tmp/scorex/perma/"
  private val DefaultAuthDataStorage = DefaultTreeDir + "authDataStorage.mapDB"

  lazy val rootHash: Array[Byte] = Array(21: Byte, 45: Byte, 11: Byte, 34: Byte, -77: Byte, -99: Byte, 105: Byte,
    62: Byte, 36: Byte, -65: Byte, 76: Byte, 59: Byte, 1: Byte, -52: Byte, 49: Byte, 78: Byte, -49: Byte, 61: Byte,
    68: Byte, 7: Byte, 73: Byte, -55: Byte, -1: Byte, -26: Byte, -17: Byte, 97: Byte, 70: Byte, -25: Byte, 109: Byte,
    -9: Byte, -112: Byte, -124: Byte)
  lazy val isTrustedDealer = (settingsJSON \ "perma" \ "isTrustedDealer").asOpt[Boolean].getOrElse(false)
  lazy val treeDir = (settingsJSON \ "perma" \ "treeDir").asOpt[String].getOrElse(DefaultTreeDir)
  lazy val authDataStorage =
    treeDir + (settingsJSON \ "perma" \ "authDataStorage").asOpt[String].getOrElse("authDataStorage.mapDB")

}
