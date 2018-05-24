package com.wavesplatform.lang.typechecker

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.CompilerV1.CompilationResult
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CompilerV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("should infer generic function return type") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val Right(v) = CompilerV1(typeCheckerContext, FUNCTION_CALL(idT.name, List(CONST_LONG(1))))
    v.tpe shouldBe LONG
  }

  property("should infer inner types") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val Right(v) =
      CompilerV1(typeCheckerContext, FUNCTION_CALL(extract.name, List(FUNCTION_CALL(undefinedOptionLong.name, List.empty))))
    v.tpe shouldBe LONG
  }

  treeTypeTest(s"unitOnNone(NONE)")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(unitOnNone.name, List(Expressions.REF("None"))),
    expectedResult = Right(FUNCTION_CALL(unitOnNone.header, List(REF("None", OPTION(NOTHING))), UNIT))
  )

  property("successful on very deep expressions(stack overflow check)") {
    val expr =
      (1 to 100000).foldLeft[Expressions.EXPR](Expressions.CONST_LONG(0))((acc, _) => Expressions.BINARY_OP(acc, SUM_OP, Expressions.CONST_LONG(1)))
    val expectedResult = Right(LONG)

    CompilerV1(typeCheckerContext, expr).map(_.tpe) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  treeTypeTest("GETTER")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.GETTER(
      ref = Expressions.REF("p"),
      field = "x"
    ),
    expectedResult = Right(
      GETTER(
        expr = REF("p", TYPEREF("Point")),
        field = "x",
        tpe = LONG
      ))
  )

  treeTypeTest("REF(OBJECT)")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.REF("p"),
    expectedResult = Right(REF("p", TYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.REF("p"),
    expectedResult = Right(REF("p", TYPEREF("Point")))
  )

  treeTypeTest("MULTIPLY(1,2)")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(multiplierFunction.name, List(Expressions.CONST_LONG(1), Expressions.CONST_LONG(2))),
    expectedResult = Right(FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(1), CONST_LONG(2)), LONG))
  )

  treeTypeTest(s"idOptionLong(NONE)")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(idOptionLong.name, List(Expressions.REF("None"))),
    expectedResult = Right(FUNCTION_CALL(idOptionLong.header, List(REF("None", OPTION(NOTHING))), UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(NONE))")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(idOptionLong.name, List(Expressions.FUNCTION_CALL("Some", List(Expressions.REF("None"))))),
    expectedResult =
      Right(FUNCTION_CALL(idOptionLong.header, List(FUNCTION_CALL(some.header, List(REF("None", OPTION(NOTHING))), OPTION(OPTION(NOTHING)))), UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(CONST_LONG(3)))")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(
      idOptionLong.name,
      List(Expressions.FUNCTION_CALL("Some", List(Expressions.FUNCTION_CALL("Some", List(Expressions.CONST_LONG(3))))))
    ),
    expectedResult = Right(
      FUNCTION_CALL(
        idOptionLong.header,
        List(FUNCTION_CALL(some.header, List(FUNCTION_CALL(some.header, List(CONST_LONG(3)), OPTION(LONG))), OPTION(OPTION(LONG)))),
        UNIT
      )
    )
  )

  treeTypeTest("Invalid LET")(
    ctx = typeCheckerContext,
    expr = Expressions.BLOCK(Expressions.LET(Expressions.PART.INVALID("###", "it is invalid!"), Expressions.TRUE, Seq.empty), Expressions.REF("x")),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("Invalid GETTER")(
    ctx = typeCheckerContext,
    expr = Expressions.GETTER(Expressions.REF("x"), Expressions.PART.INVALID("###", "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("Invalid BYTEVECTOR")(
    ctx = typeCheckerContext,
    expr = Expressions.CONST_BYTEVECTOR(Expressions.PART.INVALID("foo", "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: foo")
  )

  treeTypeTest("Invalid STRING")(
    ctx = typeCheckerContext,
    expr = Expressions.CONST_STRING(Expressions.PART.INVALID("\\u1", "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: \\u1")
  )

  treeTypeTest("Invalid REF")(
    ctx = typeCheckerContext,
    expr = Expressions.REF(Expressions.PART.INVALID("###", "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("Invalid FUNCTION_CALL")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(Expressions.PART.INVALID("###", "it is invalid!"), List.empty),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("INVALID")(
    ctx = typeCheckerContext,
    expr = Expressions.INVALID("###", None),
    expectedResult = Left("Typecheck failed: ###")
  )

  private def treeTypeTest(propertyName: String)(expr: Expressions.EXPR, expectedResult: CompilationResult[EXPR], ctx: CompilerContext): Unit =
    property(propertyName) {
      compiler.CompilerV1(ctx, expr) shouldBe expectedResult
    }

}
