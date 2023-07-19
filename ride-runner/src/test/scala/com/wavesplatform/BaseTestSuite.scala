package com.wavesplatform

import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.ride.runner.entrypoints.AppInitializer
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, OptionValues, TryValues}

import scala.reflect.ClassTag

trait BaseTestSuite extends AnyFreeSpecLike with Matchers with BeforeAndAfterAll with OptionValues with TryValues with ScorexLogging {
  protected def blockchainSettings = BaseTestSuite.settings
  protected val chainIdChar        = blockchainSettings.addressSchemeCharacter
  protected val chainId            = chainIdChar.toByte

  protected def isA[T](x: Any)(implicit ct: ClassTag[T]): T = x match {
    case x: T => x
    case _    => fail(s"Expected $x to be ${ct.runtimeClass.getSimpleName}")
  }
}

object BaseTestSuite {
  val settings = DefaultBlockchainSettings
  AppInitializer.setupChain(settings.addressSchemeCharacter)
}
