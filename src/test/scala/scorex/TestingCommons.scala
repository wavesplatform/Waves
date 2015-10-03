package scorex

import play.api.libs.json.{Json, JsObject}
import scorex.transaction.TransactionSettings


trait TestingCommons {
  val SettingsFilename = "settings-test.json"

  implicit object TestTransactionLayerSettings extends TransactionSettings{
    override val settingsJSON: JsObject = Json.obj()
  }
}
