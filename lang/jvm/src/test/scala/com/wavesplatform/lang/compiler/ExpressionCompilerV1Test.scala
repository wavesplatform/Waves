package com.wavesplatform.lang.compiler

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions.Pos
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ExpressionCompilerV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("should infer generic function return type") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val Right(v) = ExpressionCompiler(compilerContext, FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, idT.name), List(CONST_LONG(AnyPos, 1))))
    v._2 shouldBe LONG
  }

  property("should infer inner types") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val Right(v) =
      ExpressionCompiler(
        compilerContext,
        FUNCTION_CALL(AnyPos,
                      PART.VALID(AnyPos, "getElement"),
                      List(FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, returnsListLong.name), List.empty), CONST_LONG(AnyPos, 0)))
      )
    v._2 shouldBe LONG
  }

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[Expressions.EXPR](Expressions.CONST_LONG(AnyPos, 0)) { (acc, _) =>
      Expressions.BINARY_OP(AnyPos, acc, SUM_OP, Expressions.CONST_LONG(AnyPos, 1))
    }

    val expectedResult = Right(LONG)
    ExpressionCompiler(compilerContext, expr).map(_._2) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  treeTypeTest("GETTER")(
    ctx =
      CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> pointType), functionDefs = Map.empty),
    expr = Expressions.GETTER(
      AnyPos,
      ref = Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
      field = Expressions.PART.VALID(AnyPos, "x")
    ),
  expectedResult = {
      res: Either[String, (EXPR, TYPE)] => res shouldBe Right((GETTER(expr = REF("p"), field = "x"), LONG))
    }
  )

  treeTypeTest("REF(OBJECT)")(
    ctx =
      CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> pointType), functionDefs = Map.empty),
    expr = Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((REF("p"), pointType))
      }
  )

  treeTypeTest("REF x = y")(
    ctx =
      CompilerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> pointType), functionDefs = Map.empty),
    expr = Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((REF("p"), pointType))
      }
  )

  treeTypeTest("MULTIPLY(1,2)")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, multiplierFunction.name),
      List(Expressions.CONST_LONG(AnyPos, 1), Expressions.CONST_LONG(AnyPos, 2))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(1), CONST_LONG(2))), LONG))
      }
  )

  treeTypeTest("primitive getElement")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, getElement.name),
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "l")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((FUNCTION_CALL(getElement.header, List(REF("l"), CONST_LONG(1))), LONG))
      }
  )

  treeTypeTest("typeref getElement")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, getElement.name),
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "lpa")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((FUNCTION_CALL(getElement.header, List(REF("lpa"), CONST_LONG(1))), Common.pointTypeA))
      }
  )

  treeTypeTest("union getElement")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, getElement.name),
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "lpabc")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((FUNCTION_CALL(getElement.header, List(REF("lpabc"), CONST_LONG(1))), Common.AorBorC))
      }
  )

  //     let a = if (true) then 1 else ""
  //    a == 3

  treeTypeTest("union comparison")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      AnyPos,
      Expressions.LET(
        AnyPos,
        Expressions.PART.VALID(AnyPos, "a"),
        Expressions.IF(AnyPos,
                       Expressions.TRUE(AnyPos),
                       Expressions.CONST_LONG(AnyPos, 1),
                       Expressions.CONST_STRING(AnyPos, Expressions.PART.VALID(AnyPos, ""))),
        Seq.empty
      ),
      Expressions.FUNCTION_CALL(
        AnyPos,
        Expressions.PART.VALID(AnyPos, PureContext.eq.name),
        List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "a")), Expressions.CONST_LONG(AnyPos, 3))
      )
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right(
          (
            LET_BLOCK(
              LET("a", IF(TRUE, CONST_LONG(1), CONST_STRING("").explicitGet())),
              FUNCTION_CALL(PureContext.eq.header, List(REF("a"), CONST_LONG(3)))
            ),
            BOOLEAN
          )
        )
      }
  )

  treeTypeTest("idOptionLong(())")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, idOptionLong.name),
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "unit")))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((FUNCTION_CALL(idOptionLong.header, List(REF("unit"))), UNIT))
      }
  )

  treeTypeTest("pattern matching - allow shadowing of ref with the same name")(
    ctx = compilerContext,
    expr = Expressions.MATCH(
      AnyPos,
      Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
      List(
        Expressions.MATCH_CASE(
          AnyPos,
          Some(Expressions.PART.VALID(AnyPos, "p")),
          List(Expressions.PART.VALID(AnyPos, "PointA"), Expressions.PART.VALID(AnyPos, "PointB")),
          Expressions.TRUE(AnyPos)
        ),
        Expressions.MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          Expressions.FALSE(AnyPos)
        )
      )
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right(
        (LET_BLOCK(
           LET("$match0", REF("p")),
           IF(
             IF(
               FUNCTION_CALL(
                 PureContext._isInstanceOf.header,
                 List(REF("$match0"), CONST_STRING("PointB").explicitGet())
               ),
               TRUE,
               FUNCTION_CALL(
                 PureContext._isInstanceOf.header,
                 List(REF("$match0"), CONST_STRING("PointA").explicitGet())
               )
             ),
             LET_BLOCK(LET("p", REF("$match0")), TRUE),
             FALSE
           )
         ),
         BOOLEAN))
    }
  )

  treeTypeTest("pattern matching - nested matches increment tmp var name")(
    ctx = compilerContext,
    expr = {
      val script =
        """
          | let a = match p {
          |  case _ =>
          |    match p {
          |      case _ => 1
          |    }
          | }
          | let b = match p {
          |  case _ => 2
          | }
          | a + b
      """.stripMargin
      Parser.parseExpr(script).get.value
    },
    expectedResult = {
      res: Either[String, (EXPR, TYPE)] => res shouldBe Right(
        (LET_BLOCK(
           LET("a", LET_BLOCK(LET("$match0", REF("p")), LET_BLOCK(LET("$match1", REF("p")), CONST_LONG(1)))),
           LET_BLOCK(LET("b", LET_BLOCK(LET("$match0", REF("p")), CONST_LONG(2))),
                     FUNCTION_CALL(FunctionHeader.Native(100), List(REF("a"), REF("b"))))
         ),
         LONG))

    }
  )

  treeTypeTest("pattern matching - deny shadowing of other variable")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      AnyPos,
      Expressions.LET(AnyPos, Expressions.PART.VALID(AnyPos, "foo"), Expressions.TRUE(AnyPos), Seq.empty),
      Expressions.MATCH(
        AnyPos,
        Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
        List(
          Expressions.MATCH_CASE(
            AnyPos,
            Some(Expressions.PART.VALID(AnyPos, "foo")),
            List(Expressions.PART.VALID(AnyPos, "PointA"), Expressions.PART.VALID(AnyPos, "PointB")),
            Expressions.TRUE(AnyPos)
          ),
          Expressions.MATCH_CASE(
            AnyPos,
            None,
            List.empty,
            Expressions.FALSE(AnyPos)
          )
        )
      )
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: Value 'foo' already defined in the scope in -1--1")
      }
  )

  treeTypeTest("pattern matching - deny shadowing in non-ref")(
    ctx = compilerContext,
    expr = Expressions.MATCH(
      AnyPos,
      Expressions.FUNCTION_CALL(
        AnyPos,
        Expressions.PART.VALID(AnyPos, "idT"),
        List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")))
      ),
      List(
        Expressions.MATCH_CASE(
          AnyPos,
          Some(Expressions.PART.VALID(AnyPos, "p")),
          List(Expressions.PART.VALID(AnyPos, "PointA"), Expressions.PART.VALID(AnyPos, "PointB")),
          Expressions.TRUE(AnyPos)
        ),
        Expressions.MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          Expressions.FALSE(AnyPos)
        )
      )
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: Value 'p' already defined in the scope in -1--1")
      }
  )

  treeTypeTest("pattern matching - deny matching with single non-existing type")(
    ctx = compilerContext,
    expr = Expressions.MATCH(
      AnyPos,
      Expressions.FUNCTION_CALL(
        AnyPos,
        Expressions.PART.VALID(AnyPos, "idT"),
        List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")))
      ),
      List(
        Expressions.MATCH_CASE(
          AnyPos,
          Some(Expressions.PART.VALID(AnyPos, "p1")),
          List(Expressions.PART.VALID(AnyPos, "Point0"), Expressions.PART.VALID(AnyPos, "PointB")),
          Expressions.TRUE(AnyPos)
        ),
        Expressions.MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          Expressions.FALSE(AnyPos)
        )
      )
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] =>
          res.left.get.startsWith("Compilation failed: Undefined type: `Point0`, expected: PointA, PointB") shouldBe true
      }
  )

  treeTypeTest("Invalid LET")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      AnyPos,
      Expressions.LET(AnyPos, Expressions.PART.INVALID(Pos(0, 1), "can't parse"), Expressions.TRUE(AnyPos), Seq.empty),
      Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x"))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: can't parse in 0-1")
      }
  )

  treeTypeTest("Invalid GETTER")(
    ctx = compilerContext,
    expr =
      Expressions.GETTER(AnyPos, Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x")), Expressions.PART.INVALID(Pos(2, 3), "can't parse")),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: can't parse in 2-3")
      }
  )

  treeTypeTest("Invalid BYTESTR")(
    ctx = compilerContext,
    expr = Expressions.CONST_BYTESTR(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse")),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: can't parse in -1--1")
      }
  )

  treeTypeTest("Invalid STRING")(
    ctx = compilerContext,
    expr = Expressions.CONST_STRING(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse")),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: can't parse in -1--1")
      }
  )

  treeTypeTest("Invalid REF")(
    ctx = compilerContext,
    expr = Expressions.REF(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse")),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: can't parse in -1--1")
      }
  )

  treeTypeTest("Invalid FUNCTION_CALL")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse"), List.empty),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: can't parse in -1--1")
      }
  )

  treeTypeTest("INVALID")(
    ctx = compilerContext,
    expr = Expressions.INVALID(AnyPos, "###"),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: ### in -1--1")
      }
  )

  private val dropRightFunctionName: String = dropRightBytes.name

  treeTypeTest("user function overloading 1")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, dropRightFunctionName),
      List(Expressions.CONST_BYTESTR(AnyPos, Expressions.PART.VALID(AnyPos, ByteStr.empty)), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((
          FUNCTION_CALL(dropRightBytes.header, List(CONST_BYTESTR(ByteStr.empty).explicitGet(), CONST_LONG(1))),
          BYTESTR
        ))
      }
  )

  treeTypeTest("user function overloading 2")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, dropRightFunctionName),
      List(Expressions.CONST_STRING(AnyPos, Expressions.PART.VALID(AnyPos, "")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right((
          FUNCTION_CALL(dropRightString.header, List(CONST_STRING("").explicitGet(), CONST_LONG(1))),
          STRING
        ))
      }
  )

  treeTypeTest("incorrect user function overloading")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, dropRightFunctionName),
      List(Expressions.TRUE(AnyPos), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Left("Compilation failed: Can't find a function overload 'dropRight'(Boolean, Int) in -1--1")
      }
  )

  treeTypeTest("user function definition and usage")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      AnyPos,
      Expressions.FUNC(
        AnyPos,
        Expressions.PART.VALID(AnyPos, "id"),
        Seq((Expressions.PART.VALID(AnyPos, "x"), Seq((Expressions.PART.VALID(AnyPos, "Int"), None)))),
        Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x"))
      ),
      Expressions.FUNCTION_CALL(AnyPos, Expressions.PART.VALID(AnyPos, "id"), List(Expressions.CONST_LONG(AnyPos, 1L)))
    ),
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right(
          (BLOCK(
             FUNC("id", List("x"), REF("x")),
             FUNCTION_CALL(FunctionHeader.User("id"), List(CONST_LONG(1L)))
           ),
           LONG))
      }
  )

  treeTypeTest("union type inferrer with list")(
    ctx = compilerContext,
    expr = {
      val script = """[1,""]"""
      Parser.parseExpr(script).get.value
    },
    expectedResult = {
        res: Either[String, (EXPR, TYPE)] => res shouldBe Right(
          (FUNCTION_CALL(
             FunctionHeader.Native(1100),
             List(
               CONST_LONG(1),
               FUNCTION_CALL(
                 FunctionHeader.Native(1100),
                 List(
                   CONST_STRING("").explicitGet(),
                   REF("nil")
                 )
               )
             )
           ),
           LIST(UNION(List(LONG, STRING))))
        )
      }
  )

  private def treeTypeTest(propertyName: String)(expr: Expressions.EXPR, expectedResult: Either[String, (EXPR, TYPE)] => org.scalatest.compatible.Assertion, ctx: CompilerContext): Unit =
    property(propertyName) {
      expectedResult(compiler.ExpressionCompiler(ctx, expr))
    }

}
