package scorex.lagonaki

import play.api.libs.json.{JsObject, Json}
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.TransactionSettings

trait TestingCommons {
  val SettingsFilename = "settings-test.json"
  val application = new LagonakiApplication(SettingsFilename)

  implicit object TestTransactionLayerSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
  }

}