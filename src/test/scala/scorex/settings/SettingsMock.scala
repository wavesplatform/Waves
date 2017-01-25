package scorex.settings

import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsObject, Json}

abstract class SettingsMock extends WavesSettings(Json.obj()) {
  override final lazy val upnpEnabled: Boolean = false
}
