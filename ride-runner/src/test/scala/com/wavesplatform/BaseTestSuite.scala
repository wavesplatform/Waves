package com.wavesplatform

import com.wavesplatform.utils.ScorexLogging
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, OptionValues, TryValues}

trait BaseTestSuite extends AnyFreeSpecLike with Matchers with BeforeAndAfterAll with OptionValues with TryValues with ScorexLogging
