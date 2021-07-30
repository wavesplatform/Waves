package com.wavesplatform.lang.evaluator
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.test._

class TypeCastTest extends EvaluatorSpec {
  property("as") {
    eval(
      """
        | func f(a: Any) = a.as[Int]
        | f(1) == 1       &&
        | f("") == unit   &&
        | f(true) == unit &&
        | f(unit) == unit
      """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("exactAs") {
    eval(
      """
         | func f(a: Any) = a.exactAs[Int] + 1
         | f(5) == 6
      """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("exactAs error") {
    eval(
      """
         | func f(a: Any) = a.exactAs[Int]
         | f("")
      """.stripMargin
    ) shouldBe Left("Type cast error")
  }

  property("generic function error") {
    eval(
      """
         | func f(a: Any) = a.some[Int]
         | true
      """.stripMargin
    ) should produce("Can't find a function 'some'()")
  }
}
