package scorex.perma.settings

import play.api.libs.json.JsObject

trait PermaSettings {
  val settingsJSON: JsObject

  private val DefaultTreeDir = "/tmp/scorex/perma/"
  private val DefaultAuthDataStorage = DefaultTreeDir + "authDataStorage.mapDB"

  lazy val treeDir = (settingsJSON \ "perma" \ "treeDir").asOpt[String].getOrElse(DefaultTreeDir)
  lazy val authDataStorage =
    (settingsJSON \ "perma" \ "authDataStorage").asOpt[String].getOrElse(DefaultAuthDataStorage)

}
