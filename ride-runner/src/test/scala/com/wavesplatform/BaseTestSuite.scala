package com.wavesplatform

import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.ride.app.AppInitializer
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, OptionValues, TryValues}

import scala.reflect.ClassTag

trait BaseTestSuite extends AnyFreeSpecLike with Matchers with BeforeAndAfterAll with OptionValues with TryValues with ScorexLogging {
  protected def settings           = BaseTestSuite.rideRunnerSettings
  protected def blockchainSettings = settings.blockchain
  protected val chainId            = settings.blockchain.addressSchemeCharacter.toByte

  protected def isA[T](x: Any)(implicit ct: ClassTag[T]): T = x match {
    case x: T => x
    case _    => fail(s"Expected $x to be ${getSimpleName(ct.runtimeClass)}")
  }
}

object BaseTestSuite {
  val (globalConfig, rideRunnerSettings) = AppInitializer.init()
}
