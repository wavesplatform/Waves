package com.wavesplatform

import com.wavesplatform.ride.app.AppInitializer
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, OptionValues, TryValues}

trait BaseTestSuite extends AnyFreeSpecLike with Matchers with BeforeAndAfterAll with OptionValues with TryValues with ScorexLogging {
  protected def settings           = BaseTestSuite.rideRunnerSettings
  protected def blockchainSettings = settings.blockchain
  protected val chainId            = settings.blockchain.addressSchemeCharacter.toByte
}

object BaseTestSuite {
  val (globalConfig, rideRunnerSettings) = AppInitializer.init()
}
