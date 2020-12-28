package com.wavesplatform.lang.estimator

import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.utils.functionNativeCosts
import com.wavesplatform.lang.v1.estimator.v3.ContinuationFirstStepEstimator

class ContinuationFirstStepEstimatorTest extends ScriptEstimatorTestBase(ContinuationFirstStepEstimator) {
  private def estimate(script: String) =
    ContinuationFirstStepEstimator.estimate(functionNativeCosts(V4).value(), compile(script))

  property("simple let") {
    val script =
      """
        | let a = getInteger(Address(base58''), "x"+"y").value()
        | let b = 1000 + a + 10000
        | let c = a + b + 100000
        | c + a
      """.stripMargin

    /*
        cost(Address(base58'')) =  1
        cost("x"+"y")           = 20
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(31)
  }

  property("let with args") {
    val script =
      """
        | let a = 1 + 10 + getInteger(Address(base58''), "x"+"y").value()
        | let b = 1000 + a + 10000
        | let c = a + b + 100000
        | c + a
      """.stripMargin

    /*
        cost(Address(base58'')) =  1
        cost("x"+"y")           = 20
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(31)
  }

  property("if condition") {
    val script =
      """
        | if(getInteger(Address(base58''), "x"+"y").value() == 4)
        | then 3 + 1
        | else 43
      """.stripMargin

    /*
        cost(Address(base58'')) =  1
        cost("x"+"y")           = 20
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(31)
  }

  property("if with branches on true") {
    val script =
      """
        | if(2+2 == 4)
        | then 3 + getInteger(Address(base58''), "x"+"y").value()
        | else 43
      """.stripMargin

    /*
        cost(Address(base58'')) =  1
        cost("x"+"y")           = 20
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(31)
  }

  property("if with branches on false") {
    val script =
      """
        | if(2+2 == 4)
        | then 43
        | else getInteger(Address(base58''), "x"+"y")
      """.stripMargin

    /*
        cost(Address(base58'')) =  1
        cost("x"+"y")           = 20
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(31)
  }

  property("if with branches on condition, true and false") {
    val script =
      """
        | if(getInteger(Address(base58''), "x"+"y") == 4)
        | then getInteger(Address(base58''), "x"+"y")
        | else getInteger(Address(base58''), "x"+"y")
      """.stripMargin

    /*
        cost(Address(base58'')) =  1 x3
        cost("x"+"y")           = 20 x3
        cost(getInteger)        = 10 x3
     */
    estimate(script) shouldBe Right(93)
  }

  property("pure if inside getInteger") {
    val script =
      """
        | getInteger(Address(base58''), if(2+2 == 4) then "x" + "y" + "z" else "x" + "z")
      """.stripMargin

    /*
        cost(Address(base58'')) =  1
        cost(2+2)               =  1
        cost(== 4)              =  1
        cost(if)                =  1
        cost("x"+"y"+"z")       = 40
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(54)
  }

  property("expand user function") {
    val script =
      """
        | func user(s : String) = {
        |    3 + getInteger(Address(base58''), if(2+2 == 4) then s + "y" else s + "z").value()
        | }
        | 2 + user("x")
      """.stripMargin

    /*
        cost(ref(s))            =  1
        cost(Address(base58'')) =  1
        cost(2+2)               =  1
        cost(== 4)              =  1
        cost(if)                =  1
        cost(s+"y")             = 20
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(35)
  }

  property("expand predefined user function") {
    val script =
      """
        | func getIntegerValue2(addr: Address, a:String) = getInteger(addr,a).value()
        | func user(s : String) = {
        |    3 + getIntegerValue2(Address(base58''), if(2+2 == 4) then s + "y" else s + "z")
        | }
        | 2 + user("x")
      """.stripMargin

    /*
        cost(ref(s))            =  1
        cost(ref(addr))         =  1
        cost(ref(a))            =  1
        cost(Address(base58'')) =  1
        cost(2+2)               =  1
        cost(== 4)              =  1
        cost(if)                =  1
        cost(s+"y")             = 20
        cost(getInteger)        = 10
     */

    estimate(script) shouldBe Right(37)
  }

  property("expand library user function") {
    val script =
      """
        | func user(s : String) = {
        |    3 + getIntegerValue(Address(base58''), if(2+2 == 4) then s + "y" else s + "z")
        | }
        | 2 + user("x")
      """.stripMargin

    /*
      cost(ref(s))            =  1
      cost(Address(base58'')) =  1
      cost(2+2)               =  1
      cost(== 4)              =  1
      cost(if)                =  1
      cost(ref(@address))     =  1 - from expansion getIntegerValue
      cost(ref(@key))         =  1 - from expansion getIntegerValue
      cost(s+"y")             = 20
      cost(getInteger)        = 10
    */

    estimate(script) shouldBe Right(37)
  }

  property("expand user calls user") {
    val script =
      """
        | func u1(a: Int) = a + 1
        | func u2(b:Int) = u1(b) + 2
        | func u3(c:Int) = u2(c) + 3
        | func u4(d:Int) = u3(d) + 4
        | func u5(e:Int) = u4(e) + 5
        | u5(88+99) + 7
      """.stripMargin

    estimate(script) shouldBe Right(0)
  }

  property("expand user calls constructor w/ impure") {
    val script =
      """
        | func u1(a: Int) = Address(getBinary(Address(base58''), "x" + "y").value()).bytes.size() + 1
        | u1(6) + 7
      """.stripMargin

    /*
        cost(Address(base58'')) =  1
        cost("x"+"y")           = 20
        cost(getInteger)        = 10
     */
    estimate(script) shouldBe Right(31)
  }
}
