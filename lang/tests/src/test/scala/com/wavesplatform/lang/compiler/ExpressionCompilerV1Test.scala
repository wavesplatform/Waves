package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.compiler.CompilerContext.VariableInfo
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions.Pos
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, compiler}
import com.wavesplatform.lang.{Common, Global}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ExpressionCompilerV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("should infer generic function return type") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val v = ExpressionCompiler(compilerContext, FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, idT.name), List(CONST_LONG(AnyPos, 1)))).explicitGet()
    v._2 shouldBe LONG
  }

  property("should infer inner types") {
    import com.wavesplatform.lang.v1.parser.Expressions._
    val v =
      ExpressionCompiler(
        compilerContext,
        FUNCTION_CALL(
          AnyPos,
          PART.VALID(AnyPos, "getElement"),
          List(FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, returnsListLong.name), List.empty), CONST_LONG(AnyPos, 0))
        )
      ).explicitGet()
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

  property("string limit") {
    val maxString = "a" * Terms.DataEntryValueMax
    val expr      = Parser.parseExpr(s""" "$maxString" """).get.value
    ExpressionCompiler(compilerContext, expr).map(_._1) shouldBe CONST_STRING(maxString)

    val tooBigString = maxString + "a"
    val expr2        = Parser.parseExpr(s""" "$tooBigString" """).get.value
    ExpressionCompiler(compilerContext, expr2) should produce("String size=32768 exceeds 32767 bytes")

  }

  property("expression compilation fails if function name length is longer than 255 bytes") {
    val tooLongName = "a" * (ContractLimits.MaxDeclarationNameInBytes + 1)
    val funcExpr = {
      val script =
        s"""
           |func $tooLongName() = 1
           |true
        """.stripMargin
      Parser.parseExpr(script).get.value
    }
    val letExpr = {
      val script =
        s"""
           |let $tooLongName = 1
           |true
        """.stripMargin
      Parser.parseExpr(script).get.value
    }
    ExpressionCompiler(compilerContext, funcExpr) should produce(s"Function '$tooLongName' size = 256 bytes exceeds 255")
    ExpressionCompiler(compilerContext, letExpr) should produce(s"Let '$tooLongName' size = 256 bytes exceeds 255")

  }

  property("expression compiles if declaration name length is equal to 255 bytes") {
    val maxName = "a" * ContractLimits.MaxDeclarationNameInBytes
    val funcExpr = {
      val script =
        s"""
           |func $maxName() = 1
           |true
        """.stripMargin
      Parser.parseExpr(script).get.value
    }
    val letExpr = {
      val script =
        s"""
           |let $maxName = 1
           |true
        """.stripMargin
      Parser.parseExpr(script).get.value
    }
    ExpressionCompiler(compilerContext, funcExpr) shouldBe Symbol("right")
    ExpressionCompiler(compilerContext, letExpr) shouldBe Symbol("right")
  }

  property("tuple type checks") {
    val script = """ ("a", true, 123, base58'aaaa')._3 == true  """
    val expr   = Parser.parseExpr(script).get.value
    ExpressionCompiler(compilerContextV4, expr) should produce("Can't match inferred types of T over Int, Boolean")

    val script2 = """ ("a", true, 123, base58'aaaa') == ("a", true, "b", base58'aaaa') """
    val expr2   = Parser.parseExpr(script2).get.value
    ExpressionCompiler(compilerContextV4, expr2) should produce(
      "Can't match inferred types of T over (String, Boolean, Int, ByteVector), (String, Boolean, String, ByteVector)"
    )

    val script3 =
      """
        | let v = if (true) then 1 else "abc"
        | let q = if (true) then 1 else if (true) then true else "abc"
        |
        | (v, v) == (v, v)     &&
        | (v, q) == (q, v)     &&
        | (q, q) == (v, v)     &&
        | (v, q) == (1, "abc") &&
        | (v, q) == ("abc", 1) &&
        | (1, q) == (v, true)  &&
        | (((v, q), (true, v)), q) == (((1, true), (q, q)), v)
      """.stripMargin
    val expr3 = Parser.parseExpr(script3).get.value
    ExpressionCompiler(compilerContextV4, expr3) shouldBe Symbol("right")

    val script4 =
      """
        | let v = if (true) then 1 else "abc"
        | let q = if (true) then 1 else if (true) then true else "abc"
        |
        | (((v, q), (true, v)), q) == (((1, true), (v, q)), v)
      """.stripMargin
    val expr4 = Parser.parseExpr(script4).get.value
    ExpressionCompiler(compilerContextV4, expr4) should produce(
      "Can't match inferred types of T over " +
        "(((Int|String, Boolean|Int|String), (Boolean, Int|String)), Boolean|Int|String), " +
        "(((Int, Boolean), (Int|String, Boolean|Int|String)), Int|String) in 102-154"
    )
  }

  property("avoid duplicates of internal variable name of tuple destructuring") {
    val script =
      """
        | let (a1, a2, a3) = (1, 2, 3)
        | let (b1, b2, b3) = (1, 2, 3)
        | a1 == b1
      """.stripMargin
    val expr = Parser.parseExpr(script).get.value
    ExpressionCompiler(compilerContextV4, expr) shouldBe Symbol("right")
  }

  property("function with tuple args") {
    val script =
      """
        | func f(a: (Int, String), b: (String, Boolean)) =
        |   a._2 == b._1
        |
        | func g(a: (Int, (Int, String, Boolean)), b: ((Boolean, Int)|String, Boolean)) =
        |   a._2._3 == b._2
        |
        | f((1, "abc"), ("abc", true)) &&
        | g((1, (1, "abc", true)), ("abc", true))
        |
        |
      """.stripMargin
    val expr = Parser.parseExpr(script).get.value
    ExpressionCompiler(compilerContextV4, expr) shouldBe Symbol("right")

    val script2 =
      """
        | func f(a: (Int, String), b: (String, Boolean)) =
        |   a._2 == b._1
        |
        | f((1, "a"), true)
        |
      """.stripMargin
    val expr2 = Parser.parseExpr(script2).get.value
    ExpressionCompiler(compilerContextV4, expr2) should produce(
      "Non-matching types: expected: (String, Boolean), actual: Boolean in 69-86"
    )

    val script3 =
      """
        | func f(a: (Int, String, Boolean)|((Int, String), Boolean)) =
        |   a._1
        |
        | f((1, "a"))
        |
      """.stripMargin
    val expr3 = Parser.parseExpr(script3).get.value
    ExpressionCompiler(compilerContextV4, expr3) should produce(
      "Non-matching types: expected: ((Int, String), Boolean)|(Int, String, Boolean), actual: (Int, String) in 73-84"
    )
  }

  property("tuple match") {
    val script =
      """
        | let a = if (true) then (1, "abc") else ((true, 1), base58'')
        | let b = match a {
        |   case t1: (Int, String)                => t1._2
        |   case t2: ((Boolean, Int), ByteVector) => t2._1
        | }
        | let c = match b {
        |   case _: String         => true
        |   case _: (Boolean, Int) => true
        | }
        | match b {
        |   case _: (Boolean, Int) => true
        |   case _                 => true
        | }
        |
      """.stripMargin
    val expr = Parser.parseExpr(script).get.value
    ExpressionCompiler(compilerContextV4, expr) shouldBe Symbol("right")

    val script2 =
      """
        | let a = if (true) then (1, "abc") else ((true, 1), base58'')
        | let b = match a {
        |   case t1: (Int, String)              => t1._2
        |   case t2: (Boolean, Int, ByteVector) => t2._1
        | }
        |
      """.stripMargin
    val expr2 = Parser.parseExpr(script2).get.value
    ExpressionCompiler(compilerContextV4, expr2) should produce(
      "Matching not exhaustive: " +
        "possibleTypes are (Int, String), ((Boolean, Int), ByteVector), " +
        "while matched are (Int, String), (Boolean, Int, ByteVector)"
    )

    val script3 =
      """
        | let a = if (true) then 1 else ("abc", (1, true))
        | let b = if (true) then if (true) then (1, "abc") else (true, "abc") else (base58'', true, a)
        |
        | let c = match b {
        |   case t1: (Int | Boolean, String)                         => t1._1
        |   case t2: (ByteVector, Boolean, Int)                      => t2._1
        |   case t3: (ByteVector, Boolean, (String, (Int, Boolean))) => t3._1
        | }
        |
        | match b {
        |   case t1: (Int, String)                                         => t1._1
        |   case t2: (Boolean, String)                                     => t2._1
        |   case t3: (ByteVector, Boolean, Int | (String, (Int, Boolean))) => t3._1
        | }
        |
      """.stripMargin
    val expr3 = Parser.parseExpr(script3).get.value
    ExpressionCompiler(compilerContextV4, expr3) shouldBe Symbol("right")

    val script4 =
      """
        | let a = if (true) then 1 else ("abc", (1, true))
        | let b = if (true) then if (true) then (1, "abc") else (true, "abc") else (base58'', true, a)
        | match b {
        |   case t1: (Int | Boolean, String)                       => t1._1
        |   case t2: (ByteVector, Boolean, Int)                    => t2._1
        |   case t3: (ByteVector, Boolean, (String, Int, Boolean)) => t3._1
        | }
        |
      """.stripMargin
    val expr4 = Parser.parseExpr(script4).get.value
    ExpressionCompiler(compilerContextV4, expr4) should produce(
      "Matching not exhaustive: " +
        "possibleTypes are (Int, String), (Boolean, String), (ByteVector, Boolean, (String, (Int, Boolean))|Int), " +
        "while matched are (Boolean|Int, String), (ByteVector, Boolean, Int), (ByteVector, Boolean, (String, Int, Boolean)) " +
        "in 146-359"
    )

    val script5 =
      """
        | match(if true then (1, 2) else (true, "q")) {
        |   case _: (Int, Int) => true
        |   case _: (Boolean, String) => false
        | }
      """.stripMargin
    val expr5 = Parser.parseExpr(script5).get.value
    ExpressionCompiler(compilerContextV4, expr5) shouldBe Symbol("right")
  }

  property("JS API compile limit exceeding error") {
    val expr = s" ${"sigVerify(base58'', base58'', base58'') &&" * 350} true "
    val ctx = Monoid
      .combineAll(
        Seq(
          PureContext.build(V4).withEnvironment[Environment],
          CryptoContext.build(com.wavesplatform.lang.Global, V4).withEnvironment[Environment],
          WavesContext.build(
            DirectiveSet(V4, Account, Expression).explicitGet()
          )
        )
      )
      .compilerContext

    Global.compileExpression(expr, ctx, V4, ScriptEstimatorV3) should produce("Script is too large: 8756 bytes > 8192 bytes")
  }

  property("extract() removed from V4") {
    def checkExtract(version: StdLibVersion) =
      ExpressionCompiler(
        getTestContext(version).compilerContext,
        Parser.parseExpr(" extract(1) ").get.value
      )

    checkExtract(V1) shouldBe Symbol("right")
    checkExtract(V2) shouldBe Symbol("right")
    checkExtract(V3) shouldBe Symbol("right")
    checkExtract(V4) should produce("Can't find a function 'extract'")
  }

  property("DataTransaction data composite type") {
    val expr =
      """
        | match tx {
        |    case dataTx: DataTransaction =>
        |       match dataTx.data[0] {
        |         case e: BinaryEntry  => e.key == "" && e.value == base58''
        |         case e: BooleanEntry => e.key == "" && e.value == true
        |         case e: IntegerEntry => e.key == "" && e.value == 1
        |         case e: StringEntry  => e.key == "" && e.value == ""
        |         case e: DeleteEntry  => e.key == ""
        |       }
        |     case _ =>
        |       false
        | }
      """.stripMargin

    DirectiveDictionary[StdLibVersion].all
      .foreach { version =>
        val result = ExpressionCompiler(getTestContext(version).compilerContext, Parser.parseExpr(expr).get.value)
        if (version >= V4)
          result shouldBe Symbol("right")
        else
          result should produce("Undefined type: `BinaryEntry`")
      }
  }

  treeTypeTest("GETTER")(
    ctx = CompilerContext(
      predefTypes = Map(pointType.name -> pointType),
      varDefs = Map("p"                -> VariableInfo(AnyPos, pointType)),
      functionDefs = Map.empty
    ),
    expr = Expressions.GETTER(
      AnyPos,
      ref = Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
      field = Expressions.PART.VALID(AnyPos, "x")
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((GETTER(expr = REF("p"), field = "x"), LONG))
    }
  )

  treeTypeTest("REF(OBJECT)")(
    ctx = CompilerContext(
      predefTypes = Map(pointType.name -> pointType),
      varDefs = Map("p"                -> VariableInfo(AnyPos, pointType)),
      functionDefs = Map.empty
    ),
    expr = Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((REF("p"), pointType))
    }
  )

  treeTypeTest("REF x = y")(
    ctx = CompilerContext(
      predefTypes = Map(pointType.name -> pointType),
      varDefs = Map("p"                -> VariableInfo(AnyPos, pointType)),
      functionDefs = Map.empty
    ),
    expr = Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((REF("p"), pointType))
    }
  )

  treeTypeTest("MULTIPLY(1,2)")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, multiplierFunction.name),
      List(Expressions.CONST_LONG(AnyPos, 1), Expressions.CONST_LONG(AnyPos, 2))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(1), CONST_LONG(2))), LONG))
    }
  )

  treeTypeTest("primitive getElement")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, getElement.name),
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "l")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((FUNCTION_CALL(getElement.header, List(REF("l"), CONST_LONG(1))), LONG))
    }
  )

  treeTypeTest("typeref getElement")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, getElement.name),
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "lpa")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((FUNCTION_CALL(getElement.header, List(REF("lpa"), CONST_LONG(1))), Common.pointTypeA))
    }
  )

  treeTypeTest("union getElement")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, getElement.name),
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "lpabc")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((FUNCTION_CALL(getElement.header, List(REF("lpabc"), CONST_LONG(1))), Common.AorBorC))
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
        Expressions.IF(
          AnyPos,
          Expressions.TRUE(AnyPos),
          Expressions.CONST_LONG(AnyPos, 1),
          Expressions.CONST_STRING(AnyPos, Expressions.PART.VALID(AnyPos, ""))
        )
      ),
      Expressions.FUNCTION_CALL(
        AnyPos,
        Expressions.PART.VALID(AnyPos, PureContext.eq.name),
        List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "a")), Expressions.CONST_LONG(AnyPos, 3))
      )
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
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
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((FUNCTION_CALL(idOptionLong.header, List(REF("unit"))), UNIT))
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
          Expressions.FUNCTION_CALL(
            AnyPos,
            Expressions.PART.VALID(AnyPos, "=="),
            List(
              Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
              Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p"))
            )
          )
        ),
        Expressions.MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          Expressions.FALSE(AnyPos)
        )
      )
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          LET_BLOCK(
            LET("$match0", REF("p")),
            IF(
              IF(
                FUNCTION_CALL(
                  FunctionHeader.Native(FunctionIds.ISINSTANCEOF),
                  List(REF("$match0"), CONST_STRING("PointB").explicitGet())
                ),
                TRUE,
                FUNCTION_CALL(
                  FunctionHeader.Native(FunctionIds.ISINSTANCEOF),
                  List(REF("$match0"), CONST_STRING("PointA").explicitGet())
                )
              ),
              LET_BLOCK(
                LET("p", REF("$match0")),
                FUNCTION_CALL(
                  FunctionHeader.Native(FunctionIds.EQ),
                  List(REF("p"), REF("p"))
                )
              ),
              FALSE
            )
          ),
          BOOLEAN
        )
      )
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
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          LET_BLOCK(
            LET("a", LET_BLOCK(LET("$match0", REF("p")), LET_BLOCK(LET("$match1", REF("p")), CONST_LONG(1)))),
            LET_BLOCK(
              LET("b", LET_BLOCK(LET("$match0", REF("p")), CONST_LONG(2))),
              FUNCTION_CALL(FunctionHeader.Native(100), List(REF("a"), REF("b")))
            )
          ),
          LONG
        )
      )

    }
  )

  treeTypeTest("pattern matching - deny shadowing of other variable")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      AnyPos,
      Expressions.LET(AnyPos, Expressions.PART.VALID(AnyPos, "foo"), Expressions.TRUE(AnyPos)),
      Expressions.MATCH(
        AnyPos,
        Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
        List(
          Expressions.MATCH_CASE(
            AnyPos,
            Some(Expressions.PART.VALID(AnyPos, "foo")),
            List(Expressions.PART.VALID(AnyPos, "PointA"), Expressions.PART.VALID(AnyPos, "PointB")),
            Expressions.FUNCTION_CALL(
              AnyPos,
              Expressions.PART.VALID(AnyPos, "=="),
              List(
                Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "foo")),
                Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "foo"))
              )
            )
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
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("Value 'foo' already defined in the scope in -1--1")
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
          Expressions.FUNCTION_CALL(
            AnyPos,
            Expressions.PART.VALID(AnyPos, "=="),
            List(
              Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
              Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p"))
            )
          )
        ),
        Expressions.MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          Expressions.FALSE(AnyPos)
        )
      )
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("Value 'p' already defined in the scope in -1--1")
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
          None,
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
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("Undefined type: `Point0`, expected: PointA, PointB")
    }
  )

  treeTypeTest("Invalid LET")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      AnyPos,
      Expressions.LET(AnyPos, Expressions.PART.INVALID(Pos(0, 1), "can't parse"), Expressions.TRUE(AnyPos)),
      Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x"))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("can't parse in 0-1")
    }
  )

  treeTypeTest("Invalid GETTER")(
    ctx = compilerContext,
    expr =
      Expressions.GETTER(AnyPos, Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x")), Expressions.PART.INVALID(Pos(2, 3), "can't parse")),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("can't parse in 2-3")
    }
  )

  treeTypeTest("Invalid BYTESTR")(
    ctx = compilerContext,
    expr = Expressions.CONST_BYTESTR(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse")),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("can't parse in -1--1")
    }
  )

  treeTypeTest("Invalid STRING")(
    ctx = compilerContext,
    expr = Expressions.CONST_STRING(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse")),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("can't parse in -1--1")
    }
  )

  treeTypeTest("Invalid REF")(
    ctx = compilerContext,
    expr = Expressions.REF(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse")),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("can't parse in -1--1")
    }
  )

  treeTypeTest("Invalid FUNCTION_CALL")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(AnyPos, Expressions.PART.INVALID(AnyPos, "can't parse"), List.empty),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("can't parse in -1--1")
    }
  )

  treeTypeTest("INVALID")(
    ctx = compilerContext,
    expr = Expressions.INVALID(AnyPos, "###"),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("### in -1--1")
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
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          FUNCTION_CALL(dropRightBytes.header, List(CONST_BYTESTR(ByteStr.empty).explicitGet(), CONST_LONG(1))),
          BYTESTR
        )
      )
    }
  )

  treeTypeTest("user function overloading 2")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, dropRightFunctionName),
      List(Expressions.CONST_STRING(AnyPos, Expressions.PART.VALID(AnyPos, "")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          FUNCTION_CALL(dropRightString.header, List(CONST_STRING("").explicitGet(), CONST_LONG(1))),
          STRING
        )
      )
    }
  )

  treeTypeTest("incorrect user function overloading")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, dropRightFunctionName),
      List(Expressions.TRUE(AnyPos), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res should produce("Can't find a function overload 'dropRight'(Boolean, Int) in -1--1")
    }
  )

  treeTypeTest("user function definition and usage")(
    ctx = compilerContext,
    expr = Expressions.BLOCK(
      AnyPos,
      Expressions.FUNC(
        AnyPos,
        Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x")),
        Expressions.PART.VALID(AnyPos, "id"),
        Seq((Expressions.PART.VALID(AnyPos, "x"), Expressions.Single(Expressions.PART.VALID(AnyPos, "Int"), None)))
      ),
      Expressions.FUNCTION_CALL(AnyPos, Expressions.PART.VALID(AnyPos, "id"), List(Expressions.CONST_LONG(AnyPos, 1L)))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          BLOCK(
            FUNC("id", List("x"), REF("x")),
            FUNCTION_CALL(FunctionHeader.User("id"), List(CONST_LONG(1L)))
          ),
          LONG
        )
      )
    }
  )

  treeTypeTest("union type inferrer with list")(
    ctx = compilerContext,
    expr = {
      val script = """[1,""]"""
      Parser.parseExpr(script).get.value
    },
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          FUNCTION_CALL(
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
          LIST(UNION(List(LONG, STRING)))
        )
      )
    }
  )

  private def treeTypeTest(
      propertyName: String
  )(expr: Expressions.EXPR, expectedResult: Either[String, (EXPR, TYPE)] => org.scalatest.compatible.Assertion, ctx: CompilerContext): Unit =
    property(propertyName) {
      val res = compiler.ExpressionCompiler(ctx, expr)
      expectedResult(res)
    }

}
