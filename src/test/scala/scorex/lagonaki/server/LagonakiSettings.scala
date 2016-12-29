package scorex.lagonaki.server

import play.api.libs.json.JsObject
import scorex.settings.Settings
import scorex.transaction.TransactionSettings

object LagonakiSettings {
  def apply(filename: String) = new LagonakiSettings(Settings.readSettingsJson(filename))
}
class LagonakiSettings(override val settingsJSON: JsObject) extends Settings with TransactionSettings
