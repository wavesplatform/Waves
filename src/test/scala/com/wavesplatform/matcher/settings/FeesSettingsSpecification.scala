package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.{FeeSettings, FeesSettings}
import org.scalatest.{FlatSpec, Matchers}

class FeesSettingsSpecification extends FlatSpec with Matchers {
  "FeesSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees = [
        |    {transaction-type: 2, asset: WAVES, fee: 100000}
        |    {transaction-type: 3, asset: WAVES, fee: 100000000}
        |    {transaction-type: 4, asset: WAVES, fee: 100000}
        |    {transaction-type: 5, asset: WAVES, fee: 100000}
        |    {transaction-type: 6, asset: WAVES, fee: 100000}
        |    {transaction-type: 7, asset: WAVES, fee: 100000}
        |    {transaction-type: 8, asset: WAVES, fee: 100000}
        |  ]
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(7)
    settings.fees.head should be(FeeSettings(2, "WAVES", 100000))
    settings.fees.tail.head should be(FeeSettings(3, "WAVES", 100000000))
    settings.fees.tail.tail.head should be (FeeSettings(4, "WAVES", 100000))
    settings.fees.tail.tail.tail.head should be (FeeSettings(5, "WAVES", 100000))
    settings.fees.tail.tail.tail.tail.head should be (FeeSettings(6, "WAVES", 100000))
    settings.fees.tail.tail.tail.tail.tail.head should be (FeeSettings(7, "WAVES", 100000))
    settings.fees.tail.tail.tail.tail.tail.tail.head should be (FeeSettings(8, "WAVES", 100000))
  }

  it should "combine read few fees for one transaction type" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees = [
        |    {transaction-type: 1, asset: WAVES0, fee: 000}
        |    {transaction-type: 2, asset: WAVES1, fee: 111}
        |    {transaction-type: 2, asset: WAVES2, fee: 222}
        |    {transaction-type: 2, asset: WAVES3, fee: 333}
        |    {transaction-type: 3, asset: WAVES4, fee: 444}
        |  ]
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(5)
    settings.fees.head should be(FeeSettings(1, "WAVES0", 0))
    settings.fees.tail.head should be(FeeSettings(2, "WAVES1", 111))
    settings.fees.tail.tail.head should be (FeeSettings(2, "WAVES2", 222))
    settings.fees.tail.tail.tail.head should be (FeeSettings(2, "WAVES3", 333))
    settings.fees.tail.tail.tail.tail.head should be (FeeSettings(3, "WAVES4", 444))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees = []
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  fees = [
        |    {transaction-type: 1, asset: WAVES1, fee: 1111}
        |    {transaction-type: 5, asset: WAVES5, fee: 0}
        |  ]
        |}
      """.stripMargin).withFallback(
      ConfigFactory.parseString(
        """
          |waves {
          |  fees = [
          |    {transaction-type: 1, asset: WAVES1, fee: 111}
          |    {transaction-type: 2, asset: WAVES2, fee: 222}
          |    {transaction-type: 3, asset: WAVES3, fee: 333}
          |    {transaction-type: 4, asset: WAVES4, fee: 444}
          |    {transaction-type: 5, asset: WAVES5, fee: 555}
          |  ]
          |}
        """.stripMargin)
    ).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(2)
    settings.fees.head should be(FeeSettings(1, "WAVES1", 1111))
    settings.fees.tail.head should be(FeeSettings(5, "WAVES5", 0))
  }
}
