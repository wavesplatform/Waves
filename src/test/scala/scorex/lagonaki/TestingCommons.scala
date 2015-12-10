package scorex.lagonaki

import play.api.libs.json.{JsObject, Json}
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.TransactionSettings

trait TestingCommons {

  implicit object TestTransactionLayerSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
  }

}

object TestingCommons {
  val SettingsFilename = "settings-test.json"
  val application = new LagonakiApplication(SettingsFilename)
}
