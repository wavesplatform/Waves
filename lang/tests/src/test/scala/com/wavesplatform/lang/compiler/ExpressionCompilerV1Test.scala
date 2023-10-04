package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.*
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.CompilerContext.VariableInfo
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler, Terms, TestCompiler, Types}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, GlobalValNames, PureContext}
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions.Pos
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, compiler}
import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.test.*

import scala.util.Try

class ExpressionCompilerV1Test extends PropSpec {
  implicit val offset: LibrariesOffset = NoLibraries

  property("should infer generic function return type") {
    import com.wavesplatform.lang.v1.parser.Expressions.*
    val v = ExpressionCompiler(compilerContext, V3, FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, idT.name), List(CONST_LONG(AnyPos, 1)))).explicitGet()
    v._2 shouldBe LONG
  }

  property("should infer inner types") {
    import com.wavesplatform.lang.v1.parser.Expressions.*
    val v =
      ExpressionCompiler(
        compilerContext,
        V3,
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
    ExpressionCompiler(compilerContext, V3, expr).map(_._2) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  property("string limit") {
    val maxString = "a" * Terms.DataEntryValueMax
    val expr      = Parser.parseExpr(s""" "$maxString" """).get.value
    ExpressionCompiler(compilerContext, V3, expr).map(_._1) shouldBe CONST_STRING(maxString)

    val tooBigString = maxString + "a"
    val expr2        = Parser.parseExpr(s""" "$tooBigString" """).get.value
    ExpressionCompiler(compilerContext, V3, expr2) should produce("String size=32768 exceeds 32767 bytes")

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
    ExpressionCompiler(compilerContext, V3, funcExpr) should produce(s"Function '$tooLongName' size = 256 bytes exceeds 255")
    ExpressionCompiler(compilerContext, V3, letExpr) should produce(s"Let '$tooLongName' size = 256 bytes exceeds 255")

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
    ExpressionCompiler(compilerContext, V3, funcExpr) shouldBe Symbol("right")
    ExpressionCompiler(compilerContext, V3, letExpr) shouldBe Symbol("right")
  }

  property("tuple type checks") {
    val script = """ ("a", true, 123, base58'aaaa')._3 == true  """
    val expr   = Parser.parseExpr(script).get.value
    ExpressionCompiler(compilerContextV4, V4, expr) should produce("Can't match inferred types of T over Int, Boolean")

    val script2 = """ ("a", true, 123, base58'aaaa') == ("a", true, "b", base58'aaaa') """
    val expr2   = Parser.parseExpr(script2).get.value
    ExpressionCompiler(compilerContextV4, V4, expr2) should produce(
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
    ExpressionCompiler(compilerContextV4, V4, expr3) shouldBe Symbol("right")

    val script4 =
      """
        | let v = if (true) then 1 else "abc"
        | let q = if (true) then 1 else if (true) then true else "abc"
        |
        | (((v, q), (true, v)), q) == (((1, true), (v, q)), v)
      """.stripMargin
    val expr4 = Parser.parseExpr(script4).get.value
    ExpressionCompiler(compilerContextV4, V4, expr4) should produce(
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
    ExpressionCompiler(compilerContextV4, V4, expr) shouldBe Symbol("right")
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
    ExpressionCompiler(compilerContextV4, V4, expr) shouldBe Symbol("right")

    val script2 =
      """
        | func f(a: (Int, String), b: (String, Boolean)) =
        |   a._2 == b._1
        |
        | f((1, "a"), true)
        |
      """.stripMargin
    val expr2 = Parser.parseExpr(script2).get.value
    ExpressionCompiler(compilerContextV4, V4, expr2) should produce(
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
    ExpressionCompiler(compilerContextV4, V4, expr3) should produce(
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
    ExpressionCompiler(compilerContextV4, V4, expr) shouldBe Symbol("right")

    val script2 =
      """
        | let a = if (true) then (1, "abc") else ((true, 1), base58'')
        | let b = match a {
        |   case t1: (Int, String)              => t1._2
        |   case t2: (Boolean, Int, ByteVector) => t2._1
        | }
        | true
      """.stripMargin
    val expr2 = Parser.parseExpr(script2).get.value
    ExpressionCompiler(compilerContextV4, V4, expr2) should produce(
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
    ExpressionCompiler(compilerContextV4, V4, expr3) shouldBe Symbol("right")

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
    ExpressionCompiler(compilerContextV4, V4, expr4) should produce(
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
    ExpressionCompiler(compilerContextV4, V4, expr5) shouldBe Symbol("right")
  }

  property("JS API compile limit exceeding error") {
    val expr = s" ${"sigVerify(base58'', base58'', base58'') &&" * 350} true "
    val ctx = Monoid
      .combineAll(
        Seq(
          PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(com.wavesplatform.lang.Global, V4).withEnvironment[Environment],
          WavesContext.build(
            Global,
            DirectiveSet(V4, Account, Expression).explicitGet(),
            fixBigScriptField = true
          )
        )
      )
      .compilerContext

    val e = ScriptEstimatorV3(fixOverflow = true, overhead = true)
    Global.compileExpression(expr, NoLibraries, ctx, V4, Account, e) should produce("Script is too large: 8756 bytes > 8192 bytes")
  }

  property("extract() removed from V4") {
    def checkExtract(version: StdLibVersion) =
      ExpressionCompiler(
        getTestContext(version).compilerContext,
        version,
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
        val result = ExpressionCompiler(getTestContext(version).compilerContext, version, Parser.parseExpr(expr).get.value)
        if (version >= V4)
          result shouldBe Symbol("right")
        else
          result should produce("Undefined type: `BinaryEntry`")
      }
  }

  // should be works with continuations
  ignore("self-functions are unavailable for previous versions and asset scripts") {
    def expr(v: StdLibVersion, scriptType: ScriptType) = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${v.id}    #-}
           | {-# SCRIPT_TYPE    ${scriptType.value}    #-}
           | {-# CONTENT_TYPE   EXPRESSION #-}
           |
           | getInteger("key") == 1             &&
           | getIntegerValue("key") == 1        &&
           | getString("key")  == "text"        &&
           | getStringValue("key")  == "text"   &&
           | getBinary("key")  == base58''      &&
           | getBinaryValue("key")  == base58'' &&
           | getBoolean("key")  == false        &&
           | getBooleanValue("key")  == false
        """.stripMargin
      Parser.parseExpr(script).get.value
    }
    for {
      version    <- DirectiveDictionary[StdLibVersion].all
      scriptType <- DirectiveDictionary[ScriptType].all
    } {
      val result = ExpressionCompiler(getTestContext(version, scriptType).compilerContext, version, expr(version, scriptType))
      if (version < V5 || scriptType != Account)
        result.swap.getOrElse(???).split("Can't find a function").length shouldBe 9
      else
        result shouldBe Symbol("right")
    }
  }

  property("V5 functions are unavailable for previous versions") {
    def expr(v: StdLibVersion) = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${v.id}    #-}
           | {-# CONTENT_TYPE   EXPRESSION #-}
           |
           | let a = Lease(Address(base58''), 1)
           | let b = Lease(Address(base58''), 1, 0)
           | let c = calculateLeaseId(b)
           | true
        """.stripMargin
      Parser.parseExpr(script).get.value
    }

    DirectiveDictionary[StdLibVersion].all
      .foreach { version =>
        val result = ExpressionCompiler(getTestContext(version).compilerContext, version, expr(version))
        if (version < V5)
          result should produce(
            "Compilation failed: [" +
              "Can't find a function 'Lease'(Address, Int) or it is @Callable in 75-102; " +
              "Can't find a function 'Lease'(Address, Int, Int) or it is @Callable in 112-142; " +
              "Can't find a function 'calculateLeaseId'(Nothing) or it is @Callable in 152-171" +
              "]"
          )
        else
          result shouldBe Symbol("right")
      }
  }

  property("Field feeAssetId is unavailable for MassTransferTransaction") {
    def expr(v: StdLibVersion) = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${v.id}    #-}
           | {-# CONTENT_TYPE   EXPRESSION #-}
           |
           | match tx {
           |   case m: MassTransferTransaction => m.feeAssetId == unit
           |   case _                          => throw()
           | }
        """.stripMargin
      Parser.parseExpr(script).get.value
    }

    DirectiveDictionary[StdLibVersion].all
      .foreach { version =>
        val result = ExpressionCompiler(getTestContext(version).compilerContext, version, expr(version))
        if (version < V5)
          result shouldBe Symbol("right")
        else
          result should produce(
            "Compilation failed: [Undefined field `feeAssetId` of variable of type `MassTransferTransaction` in 116-128]"
          )
      }
  }

  property("Rounding modes DOWN, HALFUP, HALFEVEN, CEILING, FLOOR are available for all versions") {
    def expr(v: StdLibVersion) = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${v.id}    #-}
           | {-# CONTENT_TYPE   EXPRESSION #-}
           |
           | let r = 
           |  if (true) then
           |   DOWN 
           |  else if (true) then
           |   HALFUP 
           |  else if (true) then
           |   HALFEVEN 
           |  else if (true) then
           |   CEILING 
           |  else
           |   FLOOR
           |   
           | func f(r: Ceiling|Down|Floor|HalfEven|HalfUp) = true
           | f(r)
           |
        """.stripMargin
      Parser.parseExpr(script).get.value
    }

    DirectiveDictionary[StdLibVersion].all
      .foreach { version =>
        ExpressionCompiler(getTestContext(version).compilerContext, version, expr(version)) shouldBe Symbol("right")
      }
  }

  property("Rounding modes UP, HALFDOWN are not available from V5") {
    def expr(v: StdLibVersion) = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${v.id}    #-}
           | {-# CONTENT_TYPE   EXPRESSION #-}
           |
           | let r =
           |  if (true) then
           |   UP
           |  else
           |   HALFDOWN
           |
           | func f(r: HalfDown) = true
           | func g(r: Up) = true
           |
           | true
           |
        """.stripMargin
      Parser.parseExpr(script).get.value
    }

    DirectiveDictionary[StdLibVersion].all
      .foreach { version =>
        val result = ExpressionCompiler(getTestContext(version).compilerContext, version, expr(version))
        if (version < V5)
          result shouldBe Symbol("right")
        else {
          val error = result.swap.getOrElse(???)
          error should include("A definition of 'UP' is not found")
          error should include("Undefined type: `Up` of variable")
          error should include("A definition of 'HALFDOWN' is not found")
          error should include("Undefined type: `HalfDown` of variable")
        }
      }
  }

  property("sync invoke functions are disabled in expressions") {
    def expr(v: StdLibVersion) = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${v.id}    #-}
           | {-# CONTENT_TYPE   EXPRESSION #-}
           |
           |  let r1 = invoke(Address(base58''), "default", [], [])
           |  let r2 = reentrantInvoke(Address(base58''), "default", [], [])
           |  r1 == r2
        """.stripMargin
      Parser.parseExpr(script).get.value
    }

    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { version =>
        val result = ExpressionCompiler(getTestContext(version).compilerContext, version, expr(version))
        val error  = result.swap.getOrElse(???)
        error should include("Can't find a function 'invoke'")
        error should include("Can't find a function 'reentrantInvoke'")
      }
  }

  property("forbidden broken unicode") {
    val u1 = "\ud87e"
    Try(TestCompiler(V4).compileExpression(s""" "aaa${u1}aaa" == "${u1}aaa$u1" """)).toEither should produce(
      s"String 'aaa${u1}aaa' contains ill-formed characters in 1-10; " +
        s"String '${u1}aaa${u1}' contains ill-formed characters in 14-21"
    )
  }

  property("\\u notation is allowed") {
    val u1 = "\u0064"
    TestCompiler(V4).compileExpression(s""" "$u1" == "$u1" """) shouldBe
      TestCompiler(V4).compileExpression(s""" "d" == "d" """)
  }

  property("accessing to Any as to tuple") {
    val script =
      """
        | func f(a: Any) = a._1 == a._2
        | true
      """.stripMargin
    ExpressionCompiler.compile(script, NoLibraries, compilerContext, StdLibVersion.VersionDic.all.last) should produce(
      "Compilation failed: [" +
        "Undefined field `_1` of variable of type `Any` in 19-23; " +
        "Undefined field `_2` of variable of type `Any` in 27-31" +
        "]"
    )
  }

  property("get list element by index from variable where list is element of tuple") {
    val script =
      """
        |let t = (1, [2, 3, 4], 5)
        |let ind = 1
        |t._2[ind] == 3
        |""".stripMargin

    ExpressionCompiler.compile(script, NoLibraries, compilerContextV4, StdLibVersion.VersionDic.all.last) shouldBe Right(
      (
        LET_BLOCK(
          LET(
            "t",
            FUNCTION_CALL(
              Native(1301),
              List(
                CONST_LONG(1),
                FUNCTION_CALL(
                  Native(1100),
                  List(
                    CONST_LONG(2),
                    FUNCTION_CALL(Native(1100), List(CONST_LONG(3), FUNCTION_CALL(Native(1100), List(CONST_LONG(4), REF(GlobalValNames.Nil)))))
                  )
                ),
                CONST_LONG(5)
              )
            )
          ),
          LET_BLOCK(
            LET("ind", CONST_LONG(1)),
            FUNCTION_CALL(Native(0), List(FUNCTION_CALL(Native(401), List(GETTER(REF("t"), "_2"), REF("ind"))), CONST_LONG(3)))
          )
        ),
        Types.BOOLEAN
      )
    )
  }

  property("trying to get list element from unknown getter") {
    val script =
      """
        |let t = (1, [2, 3, 4], 5)
        |let ind = 1
        |t.some[ind] == 3
        |""".stripMargin

    ExpressionCompiler.compile(script, NoLibraries, compilerContextV4, StdLibVersion.VersionDic.all.last) should produce(
      "Compilation failed: [Non-matching types: expected: List[T], actual: Nothing in 39-50; Undefined field `some` of variable of type `(Int, List[Int], Int)` in 39-45]"
    )
  }

  property("match result of function with composite types instantiated in signature") {
    Seq(
      ("transferTransactionFromProto(base58'')", "TransferTransaction"),
      ("transferTransactionById(base58'')", "TransferTransaction"),
      ("assetInfo(base58'')", "Asset"),
      ("blockInfoByHeight(1)", "BlockInfo")
    ).foreach { case (function, resultType) =>
      DirectiveDictionary[StdLibVersion].all
        .filter(_ >= V4)
        .foreach(
          TestCompiler(_).compileExpression(
            s"""
               |match $function {
               |  case r: $resultType => r
               |  case _: Unit        => throw()
               |}
          """.stripMargin
          ) shouldBe an[ExprScript]
        )
    }
  }

  treeTypeTest("GETTER")(
    ctx = CompilerContext(
      predefTypes = Map(pointType.name -> pointType),
      varDefs = Map("p" -> VariableInfo(AnyPos, pointType)),
      functionDefs = Map.empty,
      provideRuntimeTypeOnCastError = false
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
      varDefs = Map("p" -> VariableInfo(AnyPos, pointType)),
      functionDefs = Map.empty,
      provideRuntimeTypeOnCastError = false
    ),
    expr = Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "p")),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((REF("p"), pointType))
    }
  )

  treeTypeTest("REF x = y")(
    ctx = CompilerContext(
      predefTypes = Map(pointType.name -> pointType),
      varDefs = Map("p" -> VariableInfo(AnyPos, pointType)),
      functionDefs = Map.empty,
      provideRuntimeTypeOnCastError = false
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
      List(Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, GlobalValNames.Unit)))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right((FUNCTION_CALL(idOptionLong.header, List(REF(GlobalValNames.Unit))), UNIT))
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

  treeTypeTest("user function overloading 1")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, "dropRight"),
      List(Expressions.CONST_BYTESTR(AnyPos, Expressions.PART.VALID(AnyPos, ByteStr.empty)), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          FUNCTION_CALL(User("dropRightBytes"), List(CONST_BYTESTR(ByteStr.empty).explicitGet(), CONST_LONG(1))),
          BYTESTR
        )
      )
    }
  )

  treeTypeTest("user function overloading 2")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, "dropRight"),
      List(Expressions.CONST_STRING(AnyPos, Expressions.PART.VALID(AnyPos, "")), Expressions.CONST_LONG(AnyPos, 1))
    ),
    expectedResult = { res: Either[String, (EXPR, TYPE)] =>
      res shouldBe Right(
        (
          FUNCTION_CALL(User("dropRight"), List(CONST_STRING("").explicitGet(), CONST_LONG(1))),
          STRING
        )
      )
    }
  )

  treeTypeTest("incorrect user function overloading")(
    ctx = compilerContext,
    expr = Expressions.FUNCTION_CALL(
      AnyPos,
      Expressions.PART.VALID(AnyPos, "dropRight"),
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
                  REF(GlobalValNames.Nil)
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
      val res = compiler.ExpressionCompiler(ctx, V3, expr)
      expectedResult(res)
    }

}
