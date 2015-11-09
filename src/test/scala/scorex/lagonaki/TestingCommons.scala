package scorex.lagonaki

import play.api.libs.json.{JsObject, Json}
import scorex.transaction.TransactionSettings

trait TestingCommons {
  val SettingsFilename = "settings-test.json"

  implicit object TestTransactionLayerSettings extends TransactionSettings{
    override val settingsJSON: JsObject = Json.obj()
  }
}
