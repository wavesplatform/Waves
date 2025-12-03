package com.wavesplatform.test

import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait BaseSuite extends matchers.should.Matchers, ScalaCheckPropertyChecks, ScriptGen, ShrinkLowPriority { this: Suite =>

}

abstract class FunSuite extends funsuite.AnyFunSuite, BaseSuite

abstract class FlatSpec extends flatspec.AnyFlatSpec, BaseSuite

abstract class FeatureSpec extends featurespec.AnyFeatureSpec, BaseSuite

abstract class FreeSpec extends freespec.AnyFreeSpec, BaseSuite

abstract class PropSpec extends propspec.AnyPropSpec, BaseSuite
