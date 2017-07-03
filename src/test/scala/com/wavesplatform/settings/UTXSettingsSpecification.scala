package com.wavesplatform.settings

import java.time.Duration

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class UTXSettingsSpecification extends FlatSpec with Matchers {
  "UTXSettins" should "read values" in {
    val config = ConfigFactory.parseString(
      """waves {
        |  utx {
        |    max-size = 100
        |    max-transaction-age = 100m
        |  }
        |}""".stripMargin).resolve()
    val settings = config.as[UtxSettings]("waves.utx")
    settings.maxSize should be(100)
    settings.maxTransactionAge.toMillis shouldBe Duration.ofMinutes(100).toMillis
  }
}
