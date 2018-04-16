package com.wavesplatform.lang.typechecker

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.Terms._
import com.wavesplatform.lang.v1.TypeChecker
import com.wavesplatform.lang.v1.TypeChecker.{TypeCheckResult, TypeCheckerContext}
import com.wavesplatform.lang.v1.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TypeCheckerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("should infer generic function return type") {
    import Untyped._
    val Right(v) = TypeChecker(typeCheckerContext, FUNCTION_CALL(idT.name, List(CONST_LONG(1))))
    v.tpe shouldBe LONG
  }

  property("should infer inner types") {
    import Untyped._
    val Right(v) = TypeChecker(typeCheckerContext, FUNCTION_CALL(extract.name, List(FUNCTION_CALL(undefinedOptionLong.name, List.empty))))
    v.tpe shouldBe LONG
  }

  treeTypeTest(s"unitOnNone(NONE)")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(unitOnNone.name, List(Untyped.REF("None"))),
    expectedResult = Right(Typed.FUNCTION_CALL(unitOnNone.header, List(Typed.REF("None", OPTION(NOTHING))), UNIT))
  )

  property("successful on very deep expressions(stack overflow check)") {
    val expr           = (1 to 100000).foldLeft[Untyped.EXPR](Untyped.CONST_LONG(0))((acc, _) => Untyped.BINARY_OP(acc, SUM_OP, Untyped.CONST_LONG(1)))
    val expectedResult = Right(LONG)

    TypeChecker(typeCheckerContext, expr).map(_.tpe) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  treeTypeTest("GETTER")(
    ctx = TypeCheckerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Untyped.GETTER(
      ref = Untyped.REF("p"),
      field = "x"
    ),
    expectedResult = Right(
      Typed.GETTER(
        ref = Typed.REF("p", TYPEREF("Point")),
        field = "x",
        tpe = LONG
      ))
  )

  treeTypeTest("REF(OBJECT)")(
    ctx = TypeCheckerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    ctx = TypeCheckerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  treeTypeTest("MULTIPLY(1,2)")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(multiplierFunction.name, List(Untyped.CONST_LONG(1), Untyped.CONST_LONG(2))),
    expectedResult = Right(Typed.FUNCTION_CALL(multiplierFunction.header, List(Typed.CONST_LONG(1), Typed.CONST_LONG(2)), LONG))
  )

  treeTypeTest(s"idOptionLong(NONE)")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(idOptionLong.name, List(Untyped.REF("None"))),
    expectedResult = Right(Typed.FUNCTION_CALL(idOptionLong.header, List(Typed.REF("None", OPTION(NOTHING))), UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(NONE))")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(idOptionLong.name, List(Untyped.FUNCTION_CALL("Some", List(Untyped.REF("None"))))),
    expectedResult = Right(
      Typed.FUNCTION_CALL(idOptionLong.header,
                          List(Typed.FUNCTION_CALL(some.header, List(Typed.REF("None", OPTION(NOTHING))), OPTION(OPTION(NOTHING)))),
                          UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(CONST_LONG(3)))")(
    ctx = typeCheckerContext,
    expr =
      Untyped.FUNCTION_CALL(idOptionLong.name, List(Untyped.FUNCTION_CALL("Some", List(Untyped.FUNCTION_CALL("Some", List(Untyped.CONST_LONG(3))))))),
    expectedResult = Right(
      Typed.FUNCTION_CALL(
        idOptionLong.header,
        List(Typed.FUNCTION_CALL(some.header, List(Typed.FUNCTION_CALL(some.header, List(Typed.CONST_LONG(3)), OPTION(LONG))), OPTION(OPTION(LONG)))),
        UNIT
      )
    )
  )

  private def treeTypeTest(propertyName: String)(expr: Untyped.EXPR, expectedResult: TypeCheckResult[Typed.EXPR], ctx: TypeCheckerContext): Unit =
    property(propertyName) {
      TypeChecker(ctx, expr) shouldBe expectedResult
    }

}
