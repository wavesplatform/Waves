package scorex.settings

import play.api.libs.json.{JsObject, Json}

trait SettingsMock extends Settings {
  override final lazy val settingsJSON: JsObject = Json.obj()
  override final val filename: String = ""
  override final lazy val upnpEnabled: Boolean = false
}
