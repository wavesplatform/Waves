package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CompilerV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("should infer generic function return type") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val Right(v) = CompilerV1(compilerContext, FUNCTION_CALL(0, 0, PART.VALID(0, 0, idT.name), List(CONST_LONG(0, 0, 1))))
    v._2 shouldBe LONG
  }

  property("should infer inner types") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val Right(v) =
      CompilerV1(
        compilerContext,
        FUNCTION_CALL(0, 0, PART.VALID(0, 0, extract.name), List(FUNCTION_CALL(0, 0, PART.VALID(0, 0, undefinedOptionLong.name), List.empty)))
      )
    v._2 shouldBe LONG
  }

  treeTypeTest("unitOnNone(NONE)")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(0,
                                     0,
                                     Expressions.PART.VALID(0, 0, unitOnNone.name),
                                     List(Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "None")))),
    expectedResult = Right((FUNCTION_CALL(unitOnNone.header, List(REF("None"))), UNIT))
  )

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[Expressions.EXPR](Expressions.CONST_LONG(0, 0, 0)) { (acc, _) =>
      Expressions.BINARY_OP(0, 0, acc, SUM_OP, Expressions.CONST_LONG(0, 0, 1))
    }

    val expectedResult = Right(LONG)
    CompilerV1(compilerContext, expr).map(_._2) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  treeTypeTest("GETTER")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> CASETYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.GETTER(
      0,
      0,
      ref = Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
      field = Expressions.PART.VALID(0, 0, "x")
    ),
    expectedResult = Right((GETTER(expr = REF("p"), field = "x"), LONG))
  )

  treeTypeTest("REF(OBJECT)")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> CASETYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
    expectedResult = Right((REF("p"), CASETYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    ctx = CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> CASETYPEREF("Point")), functionDefs = Map.empty),
    expr = Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
    expectedResult = Right((REF("p"), CASETYPEREF("Point")))
  )

  treeTypeTest("MULTIPLY(1,2)")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      0,
      0,
      Expressions.PART.VALID(0, 0, multiplierFunction.name),
      List(Expressions.CONST_LONG(0, 0, 1), Expressions.CONST_LONG(0, 0, 2))
    ),
    expectedResult = Right((FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(1), CONST_LONG(2))), LONG))
  )

  treeTypeTest("idOptionLong(NONE)")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      0,
      0,
      Expressions.PART.VALID(0, 0, idOptionLong.name),
      List(Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "None")))
    ),
    expectedResult = Right((FUNCTION_CALL(idOptionLong.header, List(REF("None"))), UNIT))
  )

  treeTypeTest("idOptionLong(SOME(NONE))")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      0,
      0,
      Expressions.PART.VALID(0, 0, idOptionLong.name),
      List(
        Expressions.FUNCTION_CALL(
          0,
          0,
          Expressions.PART.VALID(0, 0, "Some"),
          List(Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "None")))
        )
      )
    ),
    expectedResult = Right((FUNCTION_CALL(idOptionLong.header, List((FUNCTION_CALL(some.header, List(REF("None")))))), UNIT))
  )

  treeTypeTest("idOptionLong(SOME(CONST_LONG(3)))")(
    ctx = compilerContext,
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
      (FUNCTION_CALL(idOptionLong.header, List(FUNCTION_CALL(some.header, List(FUNCTION_CALL(some.header, List(CONST_LONG(3))))))), UNIT)
    )
  )

  treeTypeTest("pattern matching - allow shadowing of ref with the same name")(
    ctx = compilerContext,
    expr = Expressions.MATCH(
      0,
      0,
      Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
      List(
        Expressions.MATCH_CASE(
          0,
          0,
          Some(Expressions.PART.VALID(0, 0, "p")),
          List(Expressions.PART.VALID(0, 0, "PointA"), Expressions.PART.VALID(0, 0, "PointB")),
          Expressions.TRUE(0, 0)
        ),
        Expressions.MATCH_CASE(
          0,
          0,
          None,
          List.empty,
          Expressions.FALSE(0, 0)
        )
      )
    ),
    expectedResult = Right(
      (BLOCK(
         LET("$match0", REF("p")),
         IF(
           IF(
             FUNCTION_CALL(
               PureContext._isInstanceOf.header,
               List(REF("$match0"), CONST_STRING("PointB"))
             ),
             TRUE,
             FUNCTION_CALL(
               PureContext._isInstanceOf.header,
               List(REF("$match0"), CONST_STRING("PointA"))
             )
           ),
           BLOCK(LET("p", REF("$match0")), TRUE),
           FALSE
         )
       ),
       BOOLEAN))
  )

  treeTypeTest("pattern matching - deny shadowing of other variable")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      0,
      0,
      Expressions.LET(0, 0, Expressions.PART.VALID(0, 0, "foo"), Expressions.TRUE(0, 0), Seq.empty),
      Expressions.MATCH(
        0,
        0,
        Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")),
        List(
          Expressions.MATCH_CASE(
            0,
            0,
            Some(Expressions.PART.VALID(0, 0, "foo")),
            List(Expressions.PART.VALID(0, 0, "PointA"), Expressions.PART.VALID(0, 0, "PointB")),
            Expressions.TRUE(0, 0)
          ),
          Expressions.MATCH_CASE(
            0,
            0,
            None,
            List.empty,
            Expressions.FALSE(0, 0)
          )
        )
      )
    ),
    expectedResult = Left("Compilation failed: Value 'foo' already defined in the scope in 1-1")
  )

  treeTypeTest("pattern matching - deny shadowing in non-ref")(
    ctx = compilerContext,
    expr = Expressions.MATCH(
      0,
      0,
      Expressions.FUNCTION_CALL(
        0,
        0,
        Expressions.PART.VALID(0, 0, "idT"),
        List(Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "p")))
      ),
      List(
        Expressions.MATCH_CASE(
          0,
          0,
          Some(Expressions.PART.VALID(0, 0, "p")),
          List(Expressions.PART.VALID(0, 0, "PointA"), Expressions.PART.VALID(0, 0, "PointB")),
          Expressions.TRUE(0, 0)
        ),
        Expressions.MATCH_CASE(
          0,
          0,
          None,
          List.empty,
          Expressions.FALSE(0, 0)
        )
      )
    ),
    expectedResult = Left("Compilation failed: Value 'p' already defined in the scope in 1-1")
  )

  treeTypeTest("Invalid LET")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      0,
      0,
      Expressions.LET(0, 0, Expressions.PART.INVALID(0, 1, "can't parse"), Expressions.TRUE(0, 0), Seq.empty),
      Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "x"))
    ),
    expectedResult = Left("Compilation failed: can't parse in 0-1")
  )

  treeTypeTest("Invalid GETTER")(
    ctx = compilerContext,
    expr = Expressions.GETTER(0, 0, Expressions.REF(0, 0, Expressions.PART.VALID(0, 0, "x")), Expressions.PART.INVALID(2, 3, "can't parse")),
    expectedResult = Left("Compilation failed: can't parse in 2-3")
  )

  treeTypeTest("Invalid BYTEVECTOR")(
    ctx = compilerContext,
    expr = Expressions.CONST_BYTEVECTOR(0, 0, Expressions.PART.INVALID(0, 0, "can't parse")),
    expectedResult = Left("Compilation failed: can't parse in 0-0")
  )

  treeTypeTest("Invalid STRING")(
    ctx = compilerContext,
    expr = Expressions.CONST_STRING(0, 0, Expressions.PART.INVALID(0, 0, "can't parse")),
    expectedResult = Left("Compilation failed: can't parse in 0-0")
  )

  treeTypeTest("Invalid REF")(
    ctx = compilerContext,
    expr = Expressions.REF(0, 0, Expressions.PART.INVALID(0, 0, "can't parse")),
    expectedResult = Left("Compilation failed: can't parse in 0-0")
  )

  treeTypeTest("Invalid FUNCTION_CALL")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(0, 0, Expressions.PART.INVALID(0, 0, "can't parse"), List.empty),
    expectedResult = Left("Compilation failed: can't parse in 0-0")
  )

  treeTypeTest("INVALID")(
    ctx = compilerContext,
    expr = Expressions.INVALID(0, 0, "###", None),
    expectedResult = Left("Compilation failed: ### in 0-0")
  )

  private def treeTypeTest(propertyName: String)(expr: Expressions.EXPR, expectedResult: Either[String, (EXPR, TYPE)], ctx: CompilerContext): Unit =
    property(propertyName) {
      compiler.CompilerV1(ctx, expr) shouldBe expectedResult
    }

}
