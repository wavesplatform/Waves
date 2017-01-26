package scorex.settings

import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsObject, Json}

abstract class SettingsMock extends WavesSettings(Json.obj()) {
  override lazy val dataDirOpt: Option[String] = (settingsJSON \ "dataDir").asOpt[String]
    .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

  override lazy val walletDirOpt: Option[String] = (settingsJSON \ "walletDir").asOpt[String]
    .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

  override final lazy val upnpEnabled: Boolean = false
}
