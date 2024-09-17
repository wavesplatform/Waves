package com.wavesplatform.test

import com.wavesplatform.{EitherMatchers, TransactionGen}
import org.scalacheck.ShrinkLowPriority
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait BaseSuite extends matchers.should.Matchers with ScalaCheckPropertyChecks with ShrinkLowPriority with TransactionGen with EitherMatchers {
  this: Suite =>

}

abstract class FunSuite extends funsuite.AnyFunSuite with BaseSuite

abstract class FlatSpec extends flatspec.AnyFlatSpec with BaseSuite

abstract class FeatureSpec extends featurespec.AnyFeatureSpec with BaseSuite

abstract class FreeSpec extends freespec.AnyFreeSpec with BaseSuite

abstract class PropSpec extends propspec.AnyPropSpec with BaseSuite
