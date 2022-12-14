package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.directives.values.{V3, V5, V6}
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

  property("as with tuple") {
    eval(
      """
        | func f(a: Any) = a.as[(Int, String)]
        | f((1, "")) == (1, "")  &&
        | f(("", 1)) == unit     &&
        | f(1) == unit           &&
        | f("") == unit
      """.stripMargin
    )(V5) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("as with list type") {
    eval(
      """
        | func f(a: Any) = a.as[List[Any]]
        | f([""]) == [""] &&
        | f([1]) == [1]   &&
        | f([[]]) == [[]] &&
        | f([]) == []
      """.stripMargin
    )(V3) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("exactAs") {
    eval(
      """
         | func f(a: Any) = a.exactAs[Int] + 1
         | f(5) == 6
      """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("exactAs with tuple") {
    eval(
      """
        | func f(a: Any) = a.exactAs[(Int, String)]
        | f((1, "")) == (1, "")
      """.stripMargin
    )(V5) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("exactAs with list type") {
    eval(
      """
        | func f(a: Any) = a.exactAs[List[Any]]
        | f([""]) == [""] &&
        | f([1]) == [1]   &&
        | f([[]]) == [[]] &&
        | f([]) == []
      """.stripMargin
    )(V3) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("exactAs error") {
    eval(
      """
         | func f(a: Any) = a.exactAs[Int]
         | f("")
      """.stripMargin
    )(V6) shouldBe Left("String couldn't be cast to Int")
  }

  property("type cast to concrete list is not supported") {
    eval("func f(a: Any) = a.as[List[Int]]; true")(V3) should produce(
      "Type cast to List is allowed only if expecting type is List[Any] in 17-32"
    )
    eval("func f(a: Any) = a.exactAs[List[Int]]; true")(V3) should produce(
      "Type cast to List is allowed only if expecting type is List[Any] in 17-37"
    )
  }

  property("type casts in one scope") {
    eval(
      """
        |func f() = true
        |func g() = f().as[Boolean]
        |let a    = g().exactAs[Boolean] && f().exactAs[Boolean]
        |a.as[Boolean]
      """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }
}
