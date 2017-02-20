package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigException.{BadValue, NotResolved}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.{FeeSettings, FeesSettings}
import org.scalatest.{FlatSpec, Matchers}

class FeesSettingsSpecification extends FlatSpec with Matchers {
  "FeesSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  network {
        |    file = "xxx"
        |  }
        |  fees {
        |    payment {
        |      WAVES = 100000
        |    }
        |    issue {
        |      WAVES = 100000000
        |    }
        |    transfer {
        |      WAVES = 100000
        |    }
        |    reissue {
        |      WAVES = 100000
        |    }
        |    burn {
        |      WAVES = 100000
        |    }
        |    exchange {
        |      WAVES = 100000
        |    }
        |  }
        |  miner {
        |    timeout = 10
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)

    settings.fees(2) should be(List(FeeSettings("WAVES", 100000)))
    settings.fees(3) should be(List(FeeSettings("WAVES", 100000000)))
    settings.fees(4) should be(List(FeeSettings("WAVES", 100000)))
    settings.fees(5) should be(List(FeeSettings("WAVES", 100000)))
    settings.fees(6) should be(List(FeeSettings("WAVES", 100000)))
    settings.fees(7) should be(List(FeeSettings("WAVES", 100000)))
  }

  it should "combine read few fees for one transaction type" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees {
        |    payment {
        |      WAVES0 = 0
        |    }
        |    issue {
        |      WAVES1 = 111
        |      WAVES2 = 222
        |      WAVES3 = 333
        |    }
        |    transfer {
        |      WAVES4 = 444
        |    }
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(3)
    settings.fees(2).toSet should equal(Set(FeeSettings("WAVES0", 0)))
    settings.fees(3).toSet should equal(Set(FeeSettings("WAVES1", 111), FeeSettings("WAVES2", 222), FeeSettings("WAVES3", 333)))
    settings.fees(4).toSet should equal(Set(FeeSettings("WAVES4", 444)))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees {
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees.payment.WAVES1 = 1111
        |  fees.reissue.WAVES5 = 0
        |}
      """.stripMargin).withFallback(
      ConfigFactory.parseString(
        """
          |waves {
          |  fees {
          |    payment {
          |      WAVES = 100000
          |    }
          |    issue {
          |      WAVES = 100000000
          |    }
          |    transfer {
          |      WAVES = 100000
          |    }
          |    reissue {
          |      WAVES = 100000
          |    }
          |    burn {
          |      WAVES = 100000
          |    }
          |    exchange {
          |      WAVES = 100000
          |    }
          |  }
          |}
        """.stripMargin)
    ).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)
    settings.fees(2).toSet should equal(Set(FeeSettings("WAVES", 100000), FeeSettings("WAVES1", 1111)))
    settings.fees(5).toSet should equal(Set(FeeSettings("WAVES", 100000), FeeSettings("WAVES5", 0)))
  }

  it should "fail on incorrect long values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees {
        |    payment {
        |      WAVES=N/A
        |    }
        |  }
        |}
      """.stripMargin).resolve()

    intercept[BadValue] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "fail on unknown transaction type" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees {
        |    shmayment {
        |      WAVES=100
        |    }
        |  }
        |}
      """.stripMargin).resolve()

    intercept[NotResolved] {
      FeesSettings.fromConfig(config)
    }
  }
}
