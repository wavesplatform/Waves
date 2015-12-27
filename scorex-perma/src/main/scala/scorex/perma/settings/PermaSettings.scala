package scorex.perma.settings

import play.api.libs.json.JsObject

trait PermaSettings {
  val settingsJSON: JsObject

  private val DefaultTreeDir = "/tmp/scorex/perma/"
  private val DefaultAuthDataStorage = DefaultTreeDir + "authDataStorage.mapDB"

  lazy val rootHash: Array[Byte] = Array.empty
  lazy val isTrustedDealer = (settingsJSON \ "perma" \ "isTrustedDealer").asOpt[Boolean].getOrElse(false)
  lazy val treeDir = (settingsJSON \ "perma" \ "treeDir").asOpt[String].getOrElse(DefaultTreeDir)
  lazy val authDataStorage =
    treeDir + (settingsJSON \ "perma" \ "authDataStorage").asOpt[String].getOrElse("authDataStorage.mapDB")

}
