package scorex.settings

import com.wavesplatform.settings.WavesSettings
import org.scalatest.{FunSuite, Matchers}

class SettingsSpecification extends FunSuite with Matchers {
  test("UTX Size could be set in setting file") {
    val defaultSettings = new WavesSettings(Settings.readSettingsJson("settings-test.json"))
    defaultSettings.utxSize shouldBe 10000

    val settings = new WavesSettings(Settings.readSettingsJson("settings-local1.json"))
    settings.utxSize shouldBe 1234
  }
}
