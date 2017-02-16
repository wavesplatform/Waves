package scorex.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.UTXSettings
import org.scalatest.{FunSuite, Matchers}

class SettingsSpecification extends FunSuite with Matchers {

  private val defaultConfig = ConfigFactory.parseString(
    """
      |waves {
      |  utx {
      |    size: 10000
      |    broadcast-interval: 30s
      |  }
      |}
    """.stripMargin)

  private val otherConfig = ConfigFactory.parseString(
    """
      |waves {
      |  utx {
      |    size: 1234
      |    broadcast-interval: 30s
      |  }
      |}
    """.stripMargin)

  test("UTX Size could be set in setting file") {
    val defaultSettings = UTXSettings.fromConfig(defaultConfig)
    defaultSettings.size shouldBe 10000

    val settings = UTXSettings.fromConfig(otherConfig)
    settings.size shouldBe 1234
  }
}
