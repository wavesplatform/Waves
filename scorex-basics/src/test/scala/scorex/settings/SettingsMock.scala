package scorex.settings

import play.api.libs.json.{JsObject, Json}

trait SettingsMock extends Settings {
  override lazy val settingsJSON: JsObject = Json.obj()
  override val filename: String = ""
}
