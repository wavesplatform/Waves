package com.wavesplatform.lang.evaluator
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5}
import com.wavesplatform.test.produce

class ValueErrorMessageTest extends EvaluatorSpec {
  implicit val startVersion: StdLibVersion = V4

  property("by reference") {
    eval(
      s"""
         | let a = if (true) then unit else 7
         | a.value()
       """.stripMargin
    ) should produce("value() called on unit value by reference 'a'")
  }

  property("accessing field") {
    eval("SponsorFee(base58'', unit).minSponsoredAssetFee.value()") should produce(
      "value() called on unit value while accessing field 'minSponsoredAssetFee'"
    )
  }

  property("after condition") {
    eval("(if (true) then unit else 7).value()") should produce("value() called on unit value after condition evaluation")
  }

  property("after let block") {
    eval("(let a = 1; if (true) then unit else 7).value()") should produce("value() called on unit value after let block evaluation")
  }

  property("user defined function") {
    eval(
      s"""
         | func a() = if (true) then unit else 7
         | a().value()
       """.stripMargin
    ) shouldBe Left("value() called on unit value on function 'a' call")
  }

  property("data functions") {
    Seq("getIntegerValue", "getStringValue", "getBooleanValue", "getBinaryValue")
      .foreach { f =>
        eval(s"""Address(base58'abc').$f("data")""") shouldBe Left(
          s"value by key 'data' not found for the address base58'abc' on function '$f' call"
        )
        eval(s"""$f("data")""")(V5) shouldBe Left(s"value by key 'data' not found for the contract address on function '$f' call")
        eval(s"""[].$f("data")""") shouldBe Left(s"value by key 'data' not found in the list on function '$f' call")

        val unsuitable = if (f == "getIntegerValue") """BooleanEntry("data", true)""" else """IntegerEntry("data", 1)"""
        eval(s"""[$unsuitable].$f(0)""") shouldBe Left(s"value by index 0 not found in the list on function '$f' call")
      }
  }
}
