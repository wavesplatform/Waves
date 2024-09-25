package com.wavesplatform.test

import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait BaseSuite extends matchers.should.Matchers with ScalaCheckPropertyChecks with ScriptGen with ShrinkLowPriority { this: Suite => }

abstract class FunSuite extends funsuite.AnyFunSuite with BaseSuite

abstract class FlatSpec extends flatspec.AnyFlatSpec with BaseSuite

abstract class FeatureSpec extends featurespec.AnyFeatureSpec with BaseSuite

abstract class FreeSpec extends freespec.AnyFreeSpec with BaseSuite

abstract class PropSpec extends propspec.AnyPropSpec with BaseSuite
