package com.wavesplatform.mining

import com.wavesplatform.settings.TestFunctionalitySettings
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.duration._

class MinerSpec extends FreeSpec with ScalaCheckDrivenPropertyChecks {

  private val testDataGen = for {
    maxForwardOffset <- Gen.choose(1.minute, 120.minutes)
    extraOffset <- Gen.choose(1, Long.MaxValue)

  } yield TestFunctionalitySettings.Enabled.copy(maxTransactionTimeForwardOffset = maxForwardOffset)

  "Miner" - {
    "respects `maxTransactionTimeForwardOffset` setting" in {
    }
  }
}
