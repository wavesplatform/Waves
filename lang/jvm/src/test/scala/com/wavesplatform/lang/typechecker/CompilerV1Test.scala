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
    val Right(v) = CompilerV1(typeCheckerContext, FUNCTION_CALL(0, 0, PART.VALID(0, 0, idT.name), List(CONST_LONG(0, 0, 1))))
    v.tpe shouldBe LONG
  }

  property("should infer inner types") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val Right(v) =
      CompilerV1(
        typeCheckerContext,
        FUNCTION_CALL(0, 0, PART.VALID(0, 0, extract.name), List(FUNCTION_CALL(0, 0, PART.VALID(0, 0, undefinedOptionLong.name), List.empty)))
      )
    v.tpe shouldBe LONG
  }

  treeTypeTest(s"unitOnNone(NONE)")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(0,
                                     0,
                                     Expressions.PART.VALID(0, 0, unitOnNone.name),
                                     List(Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "None")))),
    expectedResult = Right(FUNCTION_CALL(unitOnNone.header, List(REF("None", OPTION(NOTHING))), UNIT))
  )

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[Expressions.EXPR](Expressions.CONST_LONG(0, 0, 0)) { (acc, _) =>
      Expressions.BINARY_OP(0, 0, acc, SUM_OP, Expressions.CONST_LONG(0, 0, 1))
    }

    val expectedResult = Right(LONG)
    CompilerV1(typeCheckerContext, expr).map(_.tpe) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  treeTypeTest("GETTER")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.GETTER(
      0,
      0,
      ref = Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
      field = Expressions.PART.VALID(0, 0, "x")
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
    expr = Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
    expectedResult = Right(REF("p", TYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
    expectedResult = Right(REF("p", TYPEREF("Point")))
  )

  treeTypeTest("MULTIPLY(1,2)")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(
      0,
      0,
      Expressions.PART.VALID(0, 0, multiplierFunction.name),
      List(Expressions.CONST_LONG(0, 0, 1), Expressions.CONST_LONG(0, 0, 2))
    ),
    expectedResult = Right(FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(1), CONST_LONG(2)), LONG))
  )

  treeTypeTest(s"idOptionLong(NONE)")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(
      0,
      0,
      Expressions.PART.VALID(0, 0, idOptionLong.name),
      List(Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "None")))
    ),
    expectedResult = Right(FUNCTION_CALL(idOptionLong.header, List(REF("None", OPTION(NOTHING))), UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(NONE))")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(
      0,
      0,
      Expressions.PART.VALID(0, 0, idOptionLong.name),
      List(
        Expressions.FUNCTION_CALL(
          0,
          0,
          Expressions.PART.VALID(0, 0, "Some"),
          List(Expressions.REF(0, 0, Expressions.PART.INVALID(0, 0, "None")))
        )
      )
    ),
    expectedResult =
      Right(FUNCTION_CALL(idOptionLong.header, List(FUNCTION_CALL(some.header, List(REF("None", OPTION(NOTHING))), OPTION(OPTION(NOTHING)))), UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(CONST_LONG(3)))")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(
      0,
      0,
      Expressions.PART.VALID(0, 0, idOptionLong.name),
      List(
        Expressions.FUNCTION_CALL(
          0,
          0,
          Expressions.PART.VALID(0, 0, "Some"),
          List(Expressions.FUNCTION_CALL(0, 0, Expressions.PART.VALID(0, 0, "Some"), List(Expressions.CONST_LONG(0, 0, 3))))
        )
      )
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
    expr = Expressions.BLOCK(
      0,
      0,
      Expressions.LET(0, 0, Expressions.PART.INVALID(0, 0, "it is invalid!"), Expressions.TRUE(0, 0), Seq.empty),
      Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "x"))
    ),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("Invalid GETTER")(
    ctx = typeCheckerContext,
    expr = Expressions.GETTER(0, 0, Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "x")), Expressions.PART.INVALID(0, 0, "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("Invalid BYTEVECTOR")(
    ctx = typeCheckerContext,
    expr = Expressions.CONST_BYTEVECTOR(0, 0, Expressions.PART.INVALID(0, 0, "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: foo")
  )

  treeTypeTest("Invalid STRING")(
    ctx = typeCheckerContext,
    expr = Expressions.CONST_STRING(0, 0, Expressions.PART.INVALID(0, 0, "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: \\u1")
  )

  treeTypeTest("Invalid REF")(
    ctx = typeCheckerContext,
    expr = Expressions.REF(0, 0, Expressions.PART.INVALID(0, 0, "it is invalid!")),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("Invalid FUNCTION_CALL")(
    ctx = typeCheckerContext,
    expr = Expressions.FUNCTION_CALL(0, 0, Expressions.PART.INVALID(0, 0, "it is invalid!"), List.empty),
    expectedResult = Left("Typecheck failed: it is invalid!: ###")
  )

  treeTypeTest("INVALID")(
    ctx = typeCheckerContext,
    expr = Expressions.INVALID(0, 0, "###", None),
    expectedResult = Left("Typecheck failed: ###")
  )

  private def treeTypeTest(propertyName: String)(expr: Expressions.EXPR, expectedResult: CompilationResult[EXPR], ctx: CompilerContext): Unit =
    property(propertyName) {
      compiler.CompilerV1(ctx, expr) shouldBe expectedResult
    }

}
