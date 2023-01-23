package com.wavesplatform.lang

import cats.Id
import cats.kernel.Monoid
import cats.syntax.either.*
import com.google.common.io.BaseEncoding
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.Keccak256
import com.wavesplatform.lang.Common.*
import com.wavesplatform.lang.Testing.*
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.{BYTESTR, FINAL, LONG}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{Contextful, ContextfulVal, EvaluatorV2}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease}
import com.wavesplatform.lang.v1.{CTX, ContractLimits}
import com.wavesplatform.test.*
import org.scalatest.Inside
import org.web3j.crypto.Keys

import java.nio.charset.StandardCharsets
import scala.util.Random

class IntegrationTest extends PropSpec with Inside {
  private def eval[T <: EVALUATED](
      code: String,
      pointInstance: Option[CaseObj] = None,
      pointType: FINAL = AorBorC,
      ctxt: CTX[NoContext] = CTX.empty,
      version: StdLibVersion = V3
  ): Either[String, T] =
    genericEval[NoContext, T](code, pointInstance, pointType, ctxt, version, Contextful.empty[Id])

  private def genericEval[C[_[_]], T <: EVALUATED](
      code: String,
      pointInstance: Option[CaseObj] = None,
      pointType: FINAL = AorBorC,
      ctxt: CTX[C],
      version: StdLibVersion,
      env: C[Id]
  ): Either[String, T] = {
    val f: BaseFunction[C] =
      NativeFunction(
        "fn1",
        1,
        91: Short,
        pointType,
        ("value", pointType)
      ) {
        case _ :: Nil => throw new Exception("test exception")
        case xs       => notImplemented[Id, EVALUATED]("fraction(value: Int, numerator: Int, denominator: Int)", xs)
      }

    val f2: BaseFunction[C] =
      NativeFunction(
        "fn2",
        1,
        92: Short,
        pointType,
        ("value", pointType)
      ) {
        case _ :: Nil => throw new SecurityException("test exception")
        case xs       => notImplemented[Id, EVALUATED]("fraction(value: Int, numerator: Int, denominator: Int)", xs)
      }

    val lazyVal       = ContextfulVal.pure[C](pointInstance.orNull)
    val stringToTuple = Map(("p", (pointType, lazyVal)))

    val ctx: CTX[C] =
      Monoid.combineAll(
        Seq(
          PureContext.build(version, useNewPowPrecision = true).withEnvironment[C],
          CryptoContext.build(Global, version).withEnvironment[C],
          addCtx.withEnvironment[C],
          CTX[C](sampleTypes, stringToTuple, Array(f, f2)),
          ctxt
        )
      )

    val compiled = ExpressionCompiler.compile(code, ctx.compilerContext)
    val evalCtx  = ctx.evaluationContext(env).asInstanceOf[EvaluationContext[Environment, Id]]
    compiled.flatMap(v =>
      EvaluatorV2
        .applyCompleted(evalCtx, v._1, LogExtraInfo(), version, correctFunctionCallScope = true, newMode = true)
        ._3
        .bimap(_.message, _.asInstanceOf[T])
    )
  }

  private val v5Ctx = WavesContext.build(Global, DirectiveSet(V5, Account, DApp).explicitGet(), fixBigScriptField = true)

  property("simple let") {
    val src =
      """
        |let a = 1
        |let b = a + a
        |b + b + b
      """.stripMargin
    eval[EVALUATED](src) shouldBe evaluated(6)
  }

  property("proper error message") {
    val src =
      """
        |match p {
        |  case _: PointA => let x = 3
        |  case _ => throw()
        |}
      """.stripMargin
    eval[EVALUATED](src) should produce("Parse error: expected expression in 39-40")
  }

  property("Exception handling") {
    val sampleScript =
      """match fn1(p) {
        |  case _: PointA => 0
        |  case _: PointB => 1
        |  case _: PointC => 2
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) should produce(
      "An error during run <fn1(value: PointA|PointB|PointC): PointA|PointB|PointC>: class java.lang.Exception test exception"
    )
  }

  property("Security Exception handling") {
    val sampleScript =
      """match fn2(p) {
        |  case _: PointA => 0
        |  case _: PointB => 1
        |  case _: PointC => 2
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) should produce(
      "An access to <fn2(value: PointA|PointB|PointC): PointA|PointB|PointC> is denied"
    )
  }

  property("patternMatching") {
    val sampleScript =
      """match p {
        |  case _: PointA => 0
        |  case _: PointB => 1
        |  case _: PointC => 2
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("patternMatching with named union types") {
    val sampleScript =
      """match p {
        |  case _: PointA => 0
        |  case _: PointBC => 1
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("union types have fields") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.YB
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(3)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(41)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(42)
  }

  property("union types have  only common fields") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.X
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointCInstance)) should produce("Compilation failed: [Undefined field `X`")
  }

  property("patternMatching _") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case _: PointB  => 1
         |  case _: PointC => 2
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("patternMatching any type") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("patternMatching block") {
    val sampleScript =
      """|
         |match (let x = 1; p) {
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("function call") {
    eval[EVALUATED]("10 + 2") shouldBe evaluated(12)
  }

  property("max values and operation order") {
    val longMax = Long.MaxValue
    val longMin = Long.MinValue
    eval(s"$longMax + 1 - 1") shouldBe Left("long overflow")
    eval(s"$longMin - 1 + 1") shouldBe Left("long overflow")
    eval(s"$longMax - 1 + 1") shouldBe evaluated(longMax)
    eval(s"$longMin + 1 - 1") shouldBe evaluated(longMin)
    eval(s"$longMax / $longMin + 1") shouldBe evaluated(0)
    eval(s"($longMax / 2) * 2") shouldBe evaluated(longMax - 1)
    eval[EVALUATED]("fraction(9223372036854775807, 3, 0)") shouldBe Left(
      s"Fraction: division by zero"
    )
    eval[EVALUATED]("fraction(9223372036854775807, 3, 2)") shouldBe Left(
      s"Long overflow: value `${BigInt(Long.MaxValue) * 3 / 2}` greater than 2^63-1"
    )
    eval[EVALUATED]("fraction(-9223372036854775807, 3, 2)") shouldBe Left(
      s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1"
    )
    eval[EVALUATED](s"$longMax + fraction(-9223372036854775807, 3, 2)") shouldBe Left(
      s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1"
    )
    eval[EVALUATED](s"2 + 2 * 2") shouldBe evaluated(6)
    eval("2 * 3 == 2 + 4") shouldBe evaluated(true)
  }

  property("equals works on primitive types") {
    eval[EVALUATED]("base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8' == base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8'") shouldBe evaluated(true)
    eval[EVALUATED]("1 == 2") shouldBe evaluated(false)
    eval[EVALUATED]("3 == 3") shouldBe evaluated(true)
    eval[EVALUATED]("false == false") shouldBe evaluated(true)
    eval[EVALUATED]("true == false") shouldBe evaluated(false)
    eval[EVALUATED]("true == true") shouldBe evaluated(true)
    eval[EVALUATED]("""   "x" == "x"     """) shouldBe evaluated(true)
    eval[EVALUATED]("""   "x" == "y"     """) shouldBe evaluated(false)
    eval[EVALUATED]("""   "x" != "y"     """) shouldBe evaluated(true)
    eval[EVALUATED]("""   "x" == 3     """) should produce("Can't match inferred types")
    eval[EVALUATED]("""   "x" != 3     """) should produce("Can't match inferred types")
    eval[EVALUATED](""" let union = if(true) then "x" else 3; union == "x"   """) shouldBe evaluated(true)
  }

  property("equals some lang structure") {
    eval[EVALUATED]("let x = (-7763390488025868909>-1171895536391400041); let v = false; (v&&true)") shouldBe evaluated(false)
    eval[EVALUATED]("let mshUmcl = (if(true) then true else true); true || mshUmcl") shouldBe evaluated(true)
    eval[EVALUATED]("""if(((1+-1)==-1)) then 1 else (1+1)""") shouldBe evaluated(2)
    eval[EVALUATED]("""((((if(true) then 1 else 1)==2)||((if(true)
                      |then true else true)&&(true||true)))||(if(((1>1)||(-1>=-1)))
                      |then (-1>=1) else false))""".stripMargin) shouldBe evaluated(true)
  }

  property("sum/mul/div/mod/fraction functions") {
    eval[EVALUATED]("(10 + 10)#jhk\n ") shouldBe evaluated(20)
    eval[EVALUATED]("(10 * 10)") shouldBe evaluated(100)
    eval[EVALUATED]("(10 / 3)") shouldBe evaluated(3)
    eval[EVALUATED]("(10 % 3)") shouldBe evaluated(1)
    eval[EVALUATED]("fraction(9223372036854775807, -2, -4)") shouldBe evaluated(Long.MaxValue / 2)
  }

  def compile(script: String): Either[String, Terms.EXPR] =
    ExpressionCompiler.compileBoolean(script, CTX.empty.compilerContext)

  property("wrong script return type") {
    compile("1") should produce("should return boolean")
    compile(""" "string" """) should produce("should return boolean")
    compile(""" base58'string' """) should produce("should return boolean")
  }

  property("equals works on elements from Gens") {
    List(CONST_LONGgen, SUMgen(50), INTGen(50)).foreach(gen =>
      forAll(for {
        (expr, res) <- gen
        str         <- toString(expr)
      } yield (str, res)) { case (str, res) =>
        withClue(str) {
          eval[EVALUATED](str) shouldBe evaluated(res)
        }
      }
    )

    forAll(for {
      (expr, res) <- BOOLgen(50)
      str         <- toString(expr)
    } yield (str, res)) { case (str, res) =>
      withClue(str) {
        eval[EVALUATED](str) shouldBe evaluated(res)
      }
    }
  }

  property("Match with not case types") {
    eval[EVALUATED]("""
                      |
                      |let a = if (true) then 1 else ""
                      |
                      |match a {
                      | case x: Int => x
                      | case _: String => 2
                      |}""".stripMargin) shouldBe evaluated(1)
  }

  property("allow unions in pattern matching") {
    val sampleScript =
      """match p {
        |  case p1: PointBC => {
        |    match p1 {
        |      case pb: PointB => pb.X
        |      case pc: PointC => pc.YB
        |    }
        |  }
        |  case _ => throw()
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(3)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(42)
  }

  property("different types, same name of field") {
    val sampleScript =
      """match (p.YB) {
        | case l: Int => l
        | case _: Unit => 1
        | }
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointCInstance), CorD) shouldBe evaluated(42)
    eval[EVALUATED](sampleScript, Some(pointDInstance1), CorD) shouldBe evaluated(43)
    eval[EVALUATED](sampleScript, Some(pointDInstance2), CorD) shouldBe evaluated(1)

    eval[EVALUATED]("p.YB", Some(pointCInstance), CorD) shouldBe evaluated(42)
    eval[EVALUATED]("p.YB", Some(pointDInstance1), CorD) shouldBe evaluated(43)
    eval[EVALUATED]("p.YB", Some(pointDInstance2), CorD) shouldBe evaluated(unit)
  }

  property("throw") {
    val script =
      """
        |let result = match p {
        |  case _: PointA => 0
        |  case _: PointB => throw()
        |  case _: PointC => throw("arrgh")
        |}
        |result
      """.stripMargin
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](script, Some(pointBInstance)) shouldBe Left("Explicit script termination")
    eval[EVALUATED](script, Some(pointCInstance)) shouldBe Left("arrgh")
  }

  property("func") {
    val script =
      """
        |func inc(z:Int) = {z + 1}
        |inc(0)
      """.stripMargin
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(1)
  }

  property("func in func") {
    val script =
      """
        |func maxx(x:Int, y: Int) = {
        |  let z = 11
        |  func max(i: Int, j:Int) = { if(i>j) then i else j }
        |  max(x,max(y,z))
        |}
        |maxx(0,10)
      """.stripMargin
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(11)
  }

  property("function overload is denied") {
    eval[EVALUATED](
      """
        |
        |func extract(x:Int, y: Int) = {
        |   4
        |}
        | extract(10)
                    """.stripMargin,
      Some(pointAInstance)
    ) should produce("already defined")
  }

  property("context won't change after inner let") {
    val script = "{ let x = 3; x } + { let x = 5; x}"
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(8)
  }

  property("contexts of different if parts do not affect each other") {
    val script = "if ({let x= 0; x > 0 }) then { let x = 3; x } else { let x = 5; x}"
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(5)
  }

  property("context won't change after execution of a user function") {
    val doubleFst = UserFunction[NoContext]("ID", 0, LONG, ("x", LONG)) {
      FUNCTION_CALL(PureContext.sumLong.header, List(REF("x"), REF("x")))
    }

    val context = Monoid.combine(
      PureContext.build(V1, useNewPowPrecision = true).evaluationContext[Id],
      EvaluationContext.build(
        typeDefs = Map.empty,
        letDefs = Map("x" -> LazyVal.fromEvaluated[Id](CONST_LONG(3L))),
        functions = Seq(doubleFst)
      )
    )

    val expr = FUNCTION_CALL(PureContext.sumLong.header, List(FUNCTION_CALL(doubleFst.header, List(CONST_LONG(1000L))), REF("x")))
    ev[CONST_LONG](context, expr) shouldBe evaluated(2003L)
  }

  property("context won't change after execution of an inner block") {
    val context = Monoid.combine(
      PureContext.build(V1, useNewPowPrecision = true).evaluationContext[Id],
      EvaluationContext.build(
        typeDefs = Map.empty,
        letDefs = Map("x" -> LazyVal.fromEvaluated[Id](CONST_LONG(3L))),
        functions = Seq()
      )
    )

    val expr = FUNCTION_CALL(
      function = PureContext.sumLong.header,
      args = List(
        BLOCK(
          dec = LET("x", CONST_LONG(5L)),
          body = REF("x")
        ),
        REF("x")
      )
    )
    ev[CONST_LONG](context, expr) shouldBe evaluated(8)
  }

  property("listN constructor primitive") {
    val src =
      """
        |cons(1, cons(2, cons(3, cons(4, cons(5, nil)))))
      """.stripMargin
    eval[EVALUATED](src) shouldBe evaluated(List(1, 2, 3, 4, 5))
  }

  property("listN constructor binary op") {
    val src =
      """
        |1::2::3::4::5::nil
      """.stripMargin
    eval[EVALUATED](src) shouldBe evaluated(List(1, 2, 3, 4, 5))
  }

  property("list syntax sugar") {
    val src =
      """
        |[1,2,3, 4, 5]
      """.stripMargin
    eval[EVALUATED](src) shouldBe evaluated(List(1, 2, 3, 4, 5))
  }

  property("LIST access IndexOutOfBounds") {
    val src =
      """
        |[1].getElement(1)
      """.stripMargin
    eval(src) should produce("Index 1 out of bounds for length 1")

    def testListAccessError(length: Int, index: Int): Unit = {
      eval(
        s"""
           | let a = ${List.fill(length)(1).mkString("[", ", ", "]")}
           | a[$index]
         """.stripMargin
      ) should produce(s"Index $index out of bounds for length $length")
    }

    testListAccessError(length = 3, index = 3)
    testListAccessError(length = 3, index = -1)
    testListAccessError(length = 0, index = 1)
  }

  property("list constructor for different data entries") {
    val src =
      """
        |let x = DataEntry("foo",1)
        |let y = DataEntry("bar","2")
        |let z = DataEntry("baz","2")
        |[x,y,z]
      """.stripMargin
    eval[EVALUATED](src) shouldBe Right(
      ARR(
        Vector(
          CaseObj(
            dataEntryType,
            Map(
              "key"   -> CONST_STRING("foo").explicitGet(),
              "value" -> CONST_LONG(1)
            )
          ),
          CaseObj(
            dataEntryType,
            Map(
              "key"   -> CONST_STRING("bar").explicitGet(),
              "value" -> CONST_STRING("2").explicitGet()
            )
          ),
          CaseObj(
            dataEntryType,
            Map(
              "key"   -> CONST_STRING("baz").explicitGet(),
              "value" -> CONST_STRING("2").explicitGet()
            )
          )
        ),
        false
      ).explicitGet()
    )
  }

  property("allow 'throw' in '==' arguments") {
    val src =
      """true == throw("test passed")"""
    eval[EVALUATED](src) shouldBe Left("test passed")
  }

  property("ban to compare different types") {
    val src =
      """true == "test passed" """
    eval[EVALUATED](src) should produce("Compilation failed: [Can't match inferred types")
  }

  property("postfix syntax (one argument)") {
    val src =
      """
        | let list = [1, 2, 3]
        | list.getElement(1)
      """.stripMargin

    eval[EVALUATED](src) shouldBe Right(CONST_LONG(2))
  }

  property("postfix syntax (no arguments)") {
    val src =
      """unit.isDefined()"""
    eval[EVALUATED](src) shouldBe Right(FALSE)
  }

  property("postfix syntax (many argument)") {
    val src =
      """ 5.fraction(7,2) """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(17L))
  }

  property("postfix syntax (users defined function)") {
    val src =
      """
        |func dub(s:String) = { s+s }
        |"qwe".dub()
      """.stripMargin
    eval[EVALUATED](src) shouldBe CONST_STRING("qweqwe")
  }

  property("extract UTF8 string") {
    val src =
      """ base58'2EtvziXsJaBRS'.toUtf8String() """
    eval[EVALUATED](src) shouldBe CONST_STRING("abcdefghi")
  }

  property("toInt from ByteVector") {
    val src =
      """ base58'2EtvziXsJaBRS'.toInt() """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(0x6162636465666768L))
  }

  property("toInt with explicit zero offset") {
    val src =
      """ base58'2EtvziXsJaBRS'.toInt(0) """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(0x6162636465666768L))
  }

  property("toInt by offset") {
    val src =
      """ base58'2EtvziXsJaBRS'.toInt(1) """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(0x6263646566676869L))
  }

  property("toInt from end of max sized ByteVector by offset") {
    val array = new Array[Byte](65536)
    for (i <- 65528 to 65535) array(i) = 1
    val src =
      s""" arr.toInt(65528) """
    val arrVal = ContextfulVal.pure[NoContext](CONST_BYTESTR(ByteStr(array), limit = CONST_BYTESTR.DataTxSize).explicitGet())
    eval[EVALUATED](
      src,
      ctxt = CTX[NoContext](
        Seq(),
        Map("arr" -> (BYTESTR -> arrVal)),
        Array()
      )
    ) shouldBe Right(CONST_LONG(0x0101010101010101L))
  }

  property("toInt by offset (partial)") {
    val src =
      """ base58'2EtvziXsJaBRS'.toInt(2) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("toInt by offset (out of bounds)") {
    val src =
      """ base58'2EtvziXsJaBRS'.toInt(10) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("toInt by Long.MaxValue offset (out of bounds)") {
    val src =
      s""" base58'2EtvziXsJaBRS'.toInt(${Long.MaxValue}) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("toInt by offset (negative)") {
    val src =
      """ base58'2EtvziXsJaBRS'.toInt(-1) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("toInt by offset (negative Long.MinValue)") {
    val src =
      s""" base58'2EtvziXsJaBRS'.toInt(${Long.MinValue}) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("toInt from < 8 bytes (Buffer underflow)") {
    val src =
      """ "AAAAAAA".toBytes().toInt() """
    eval[EVALUATED](src) should produce("Buffer underflow")
  }

  property("toInt from < 8 bytes by zero offset (Buffer underflow)") {
    val src =
      """ "AAAAAAA".toBytes().toInt(0) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("toInt from 0 bytes (Buffer underflow)") {
    val src =
      """ "".toBytes().toInt() """
    eval[EVALUATED](src) should produce("Buffer underflow")
  }

  property("toInt from 0 bytes by zero offset (Buffer underflow)") {
    val src =
      """ "".toBytes().toInt(0) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("toInt from 0 bytes by offset (IndexOutOfBounds)") {
    val src =
      """ "".toBytes().toInt(1) """
    eval[EVALUATED](src) should produce("IndexOutOfBounds")
  }

  property("split") {
    val src =
      """ "q:we:.;q;we:x;q.we".split(":.;") """
    eval[EVALUATED](src) shouldBe Right(
      ARR(
        IndexedSeq(
          CONST_STRING("q:we").explicitGet(),
          CONST_STRING("q;we:x;q.we").explicitGet()
        ),
        false
      ).explicitGet()
    )
  }

  property("split separate correctly") {
    val src =
      """ "str1;str2;str3;str4".split(";") """
    eval[EVALUATED](src) shouldBe Right(
      ARR(
        IndexedSeq(
          CONST_STRING("str1").explicitGet(),
          CONST_STRING("str2").explicitGet(),
          CONST_STRING("str3").explicitGet(),
          CONST_STRING("str4").explicitGet()
        ),
        false
      ).explicitGet()
    )
  }

  property("split separator at the end") {
    val src =
      """ "str1;str2;".split(";") """
    eval[EVALUATED](src) shouldBe Right(
      ARR(
        IndexedSeq(
          CONST_STRING("str1").explicitGet(),
          CONST_STRING("str2").explicitGet(),
          CONST_STRING("").explicitGet()
        ),
        false
      ).explicitGet()
    )
  }

  property("split double separator") {
    val src =
      """ "str1;;str2;str3".split(";") """
    eval[EVALUATED](src) shouldBe Right(
      ARR(
        IndexedSeq(
          CONST_STRING("str1").explicitGet(),
          CONST_STRING("").explicitGet(),
          CONST_STRING("str2").explicitGet(),
          CONST_STRING("str3").explicitGet()
        ),
        false
      ).explicitGet()
    )
  }

  property("parseInt") {
    val src =
      """ "42".parseInt() """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(42L))
  }

  property("parseInt Long.MaxValue") {
    val num = Long.MaxValue
    val src =
      s""" "${num.toString}".parseInt() """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(num))
  }

  property("parseInt Long.MinValue") {
    val num = Long.MinValue
    val src =
      s""" "${num.toString}".parseInt() """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(num))
  }

  property("parseIntValue") {
    val src =
      """ "42".parseIntValue() """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(42L))
  }

  property("parseIntValue Long.MaxValue") {
    val num = Long.MaxValue
    val src =
      s""" "${num.toString}".parseIntValue() """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(num))
  }

  property("parseIntValue Long.MinValue") {
    val num = Long.MinValue
    val src =
      s""" "${num.toString}".parseIntValue() """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(num))
  }

  property("parseInt fail") {
    val src =
      """ "x42".parseInt() """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("parseIntValue fail when string starts with non-digit") {
    val src =
      """ "x42".parseIntValue() """
    eval[EVALUATED](src) shouldBe Symbol("left")
  }

  property("parseInt fail when string ends with non-digit") {
    val src =
      """ "42x".parseInt() """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("parseIntValue fail when string ends with non-digit") {
    val src =
      """ "42x".parseIntValue() """
    eval[EVALUATED](src) shouldBe Symbol("left")
  }

  property("parseInt fail when string is empty") {
    val src =
      """ "".parseInt() """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("parseIntValue fail when string is empty") {
    val src =
      """ "".parseIntValue() """
    eval[EVALUATED](src) shouldBe Symbol("left")
  }

  property("matching case with non-existing type") {
    val sampleScript =
      """|
         | let a = if (true) then 1 else "str"
         | match a {
         |   case _: UndefinedType => 0
         |   case _                => 1
         | }
         |
      """.stripMargin
    eval(sampleScript) should produce("Undefined type: `UndefinedType` of variable `a`, expected: Int, String")
  }

  property("big let assignment chain") {
    val count = 5000
    val script =
      s"""
         | let a0 = 1
         | ${1 to count map (i => s"let a$i = a${i - 1}") mkString "\n"}
         | a$count == a$count
      """.stripMargin

    eval[EVALUATED](script, None) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("big function assignment chain") {
    val count = 2000
    val script =
      s"""
         | func a0() = {
         |   1 + 1
         | }
         | ${1 to count map (i => s"func a$i() = a${i - 1}()") mkString "\n"}
         | a$count() == a$count()
      """.stripMargin

    eval[EVALUATED](script, None) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("big let assignment chain with function") {
    val count = 5000
    val script =
      s"""
         | let a0 = 1
         | ${1 to count map (i => s"let a$i = a${i - 1} + 1") mkString "\n"}
         | a$count == a$count
      """.stripMargin

    eval[EVALUATED](script, None) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("rounding modes") {
    eval[EVALUATED]("Down() == DOWN", None, version = V4) should produce("Can't find a function")
    eval[EVALUATED]("Up() == UP", None, version = V4) should produce("Can't find a function")
    eval[EVALUATED]("Ceiling() == CEILING", None) should produce("Can't find a function")
    eval[EVALUATED]("HalfUp() == HALFUP", None) should produce("Can't find a function")
    eval[EVALUATED]("HalfDown() == HALFDOWN", None) should produce("Can't find a function")
    eval[EVALUATED]("HalfEven() == HALFEVEN", None) should produce("Can't find a function")
  }

  property("RSA hash algorithms") {
    eval[EVALUATED]("NoAlg() == NOALG", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Md5() == MD5", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha1() == SHA1", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha224() == SHA224", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha256() == SHA256", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha384() == SHA384", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha512() == SHA512", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha3224() == SHA3224", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha3256() == SHA3256", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha3384() == SHA3384", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("Sha3512() == SHA3512", None) shouldBe Right(CONST_BOOLEAN(true))

    eval[EVALUATED]("NoAlg() != SHA224", None) should produce("Can't match inferred types")
    eval[EVALUATED]("MD5 != SHA3224", None) should produce("Can't match inferred types")
    eval[EVALUATED]("Sha512() != Sha3512()", None) should produce("Can't match inferred types")

    eval[EVALUATED]("MD5 == if true then MD5 else SHA1", None) shouldBe Right(CONST_BOOLEAN(true))
    eval[EVALUATED]("MD5 == if true then SHA1 else MD5", None) shouldBe Right(CONST_BOOLEAN(false))
  }

  property("HalfUp is type") {
    eval("let r = if true then HALFUP else HALFDOWN ; match r { case _:HalfUp => 1 case _ => 0 }") shouldBe Right(CONST_LONG(1))
  }

  property("HalfUp type have no constructor") {
    eval("pow(10, 0, -8, 0, 8, HalfUp())") shouldBe Symbol("Left")
  }

  property("concat empty list") {
    val script =
      s"""
         | let l = if (true) then cons(1, nil) else nil
         | let concat = 0 :: l
         | concat == [0, 1]
         |
      """.stripMargin

    eval[EVALUATED](script, None) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("list type inferrer") {
    val script =
      s"""
         | let l = if (true) then [1] else ["str"]
         | let n = "qqq" :: l
         | match n[1] {
         |   case i: Int => i == 1
         |   case _: String => false
         | } &&
         | match n[0] {
         |   case _: Int => false
         |   case s: String => s == "qqq"
         | }
      """.stripMargin

    eval[EVALUATED](script, None) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("matching parameterized types") {
    val script =
      s"""
         | func dosSigVerify() = {
         |    let result = if true then [DataEntry("a", "a")] else ""
         |    let entry = match result[0] {
         |        case r:DataEntry => r
         |        case _ => throw("err")
         |    }
         |    WriteSet(result)
         | }
         | true
      """.stripMargin

    eval[EVALUATED](script, None) should produce("expected: List[T], actual: List[DataEntry]|String")
  }

  property("extract functions with message") {
    val message = "Custom error message"
    def script(error: Boolean): String =
      s"""
         |
         | let a = if ($error) then unit else 1
         | valueOrErrorMessage(a, "$message")
         |
       """.stripMargin

    eval(script(error = false)) shouldBe Right(CONST_LONG(1))
    eval(script(error = true)) shouldBe Left(message)
  }

  property("list as argument") {
    val script =
      """
        | func head(a: List[Int]) = [a[2], a[0]]
        | head([1, 2, 3]) == [3, 1]
      """.stripMargin
    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("illegal generic") {
    val script =
      """
        | func head(a: Generic[Int]) = a
        | true
      """.stripMargin
    eval(script) should produce("Undefined generic type")
  }

  property("list type inferrer 2") {
    val script =
      s"""
         | let l = if (true)
         |         then if false
         |              then nil
         |              else nil
         |         else "str"
         | size(l) == 0
      """.stripMargin

    eval[EVALUATED](script, None) should produce("Can't find a function overload 'size'")
  }

  property("valueOrElse") {
    val script =
      s"""
         | let a = if (true) then 1 else unit
         | let b = if (false) then 2 else unit
         | let c = 3
         |
         | a.valueOrElse(b) == 1          &&
         | b.valueOrElse(a) == 1          &&
         | a.valueOrElse(c) == 1          &&
         | b.valueOrElse(c) == c          &&
         | c.valueOrElse(a) == c          &&
         | b.valueOrElse(b) == unit       &&
         | unit.valueOrElse(unit) == unit
      """.stripMargin

    eval(script, version = V3) should produce("Can't find a function")
    eval(script, version = V4) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("list append") {
    val script =
      """
        | [1, 2, 3, 4] :+ 5 == [1, 2, 3, 4, 5] &&
        | 1 :: [] :+ 2 :+ 3 == [1, 2, 3]
      """.stripMargin

    eval(script, version = V3) should produce("Can't find a function")
    eval(script, version = V4) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("list concat") {
    val script =
      """
        | [1, 2, 3, 4] ++ [5, 6] == [1, 2, 3, 4, 5, 6] &&
        | nil ++ [1, 2] :+ 3 ++ nil == [1, 2, 3]
      """.stripMargin

    eval(script, version = V3) should produce("Can't find a function")
    eval(script, version = V4) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("list result size limit") {
    val maxLongList = "[1" + ",1" * (PureContext.MaxListLengthV4 - 1) + "]"
    val consScript =
      s"""
         | let list1 = $maxLongList
         | let list2 = 1 :: list1
         | list2.size() == ${MaxListLengthV4 + 1}
       """.stripMargin

    eval(consScript, version = V3) shouldBe Right(CONST_BOOLEAN(true))
    eval(consScript, version = V4) should produce(s"exceed $MaxListLengthV4")

    val appendScript = s"[1] ++ $maxLongList"
    eval(appendScript, version = V4) should produce(s"exceed $MaxListLengthV4")

    val concatScript = s"$maxLongList :+ 1"
    eval(concatScript, version = V4) should produce(s"exceed $MaxListLengthV4")
  }

  property("callable V3 syntax absent at V4") {
    val writeSetScript =
      s"""
         | WriteSet([DataEntry("key1", "value"), DataEntry("key2", true)])
       """.stripMargin

    val transferSetScript =
      s"""
         | let tsArr = [
         |   ScriptTransfer(Address(base58'aaaa'), 100, unit),
         |   ScriptTransfer(Address(base58'bbbb'), 2,   base58'xxx')
         | ]
         | let ts = TransferSet(tsArr)
         | true
       """.stripMargin

    val scriptResultScript =
      s"""
         | func f(sr: ScriptResult) = true
         | true
         |
       """.stripMargin

    val ctx = WavesContext.build(Global, DirectiveSet(V4, Account, DApp).explicitGet(), fixBigScriptField = true)

    genericEval[Environment, EVALUATED](
      writeSetScript,
      ctxt = ctx,
      version = V4,
      env = utils.environment
    ) should produce("Can't find a function 'WriteSet'")

    genericEval[Environment, EVALUATED](
      transferSetScript,
      ctxt = ctx,
      version = V4,
      env = utils.environment
    ) should produce("Can't find a function 'TransferSet'")

    genericEval[Environment, EVALUATED](
      scriptResultScript,
      ctxt = ctx,
      version = V4,
      env = utils.environment
    ) should produce("Undefined type")
  }

  property("List[Int] median - 100 elements") {
    val arr       = (1 to 100).map(_ => Random.nextLong())
    val arrSorted = arr.sorted
    val src =
      s"[${arr.mkString(",")}].median()"
    eval(src, version = V4) shouldBe Right(CONST_LONG(Math.floorDiv(arrSorted(49) + arrSorted(50), 2)))
  }

  property("List[Int] median - 99 elements") {
    val arr       = (1 to 99).map(_ => Random.nextLong())
    val arrSorted = arr.sorted
    val src =
      s"[${arr.mkString(",")}].median()"
    eval(src, version = V4) shouldBe Right(CONST_LONG(arrSorted(49)))
  }

  property("List[Int] median - 1 elements") {
    val arr = Seq(Random.nextLong())
    val src =
      s"[${arr.mkString(",")}].median()"
    eval(src, version = V4) shouldBe Right(CONST_LONG(arr.head))
  }

  property("List[Int] median - negative rounding down") {
    val arr = Seq(3, -8)
    val src =
      s"[${arr.mkString(",")}].median()"
    eval(src, version = V4) shouldBe Right(CONST_LONG(-3))
  }

  property("List[Int] median - 1000 elements - success") {
    val src =
      s"[${(1 to 1000).mkString(",")}].median()"
    eval(src, version = V4) shouldBe Right(CONST_LONG(Math.floorDiv(500 + 501, 2)))
  }

  property("List[Int] median - empty list - error") {
    val src =
      s"[].median()"
    eval(src, version = V4) should produce("Can't find median for empty list")
  }

  property("List[Int] median - list with non int elements - error") {
    val src =
      s"""["1", "2"].median()"""
    eval(src, version = V4) should produce("Compilation failed: [Non-matching types: expected: List[Int], actual: List[String]")
  }

  property("List[Int] median - list with big elements - error") {
    val src =
      s"""[${Long.MaxValue}, ${Long.MaxValue - 2}].median()"""
    eval(src, version = V4) shouldBe Right(CONST_LONG(Long.MaxValue - 1))
  }

  property("List[Int] median - list with MinValue - error") {
    val src =
      s"""[0, ${Long.MinValue}].median()"""
    eval(src, version = V4) shouldBe Right(CONST_LONG(Long.MinValue / 2))
  }

  property("groth16Verify") {
    val src =
      s"""
         | let key = base64'hwk883gUlTKCyXYA6XWZa8H9/xKIYZaJ0xEs0M5hQOMxiGpxocuX/8maSDmeCk3bo5ViaDBdO7ZBxAhLSe5k/5TFQyF5Lv7KN2tLKnwgoWMqB16OL8WdbePIwTCuPtJNAFKoTZylLDbSf02kckMcZQDPF9iGh+JC99Pio74vDpwTEjUx5tQ99gNQwxULtztsqDRsPnEvKvLmsxHt8LQVBkEBm2PBJFY+OXf1MNW021viDBpR10mX4WQ6zrsGL5L0GY4cwf4tlbh+Obit+LnN/SQTnREf8fPpdKZ1sa/ui3pGi8lMT6io4D7Ujlwx2RdCkBF+isfMf77HCEGsZANw0hSrO2FGg14Sl26xLAIohdaW8O7gEaag8JdVAZ3OVLd5Df1NkZBEr753Xb8WwaXsJjE7qxwINL1KdqA4+EiYW4edb7+a9bbBeOPtb67ZxmFqgyTNS/4obxahezNkjk00ytswsENg//Ee6dWBJZyLH+QGsaU2jO/W4WvRyZhmKKPdipOhiz4Rlrd2XYgsfHsfWf5v4GOTL+13ZB24dW1/m39n2woJ+v686fXbNW85XP/r'
         | let proof = base64'lvQLU/KqgFhsLkt/5C/scqs7nWR+eYtyPdWiLVBux9GblT4AhHYMdCgwQfSJcudvsgV6fXoK+DUSRgJ++Nqt+Wvb7GlYlHpxCysQhz26TTu8Nyo7zpmVPH92+UYmbvbQCSvX2BhWtvkfHmqDVjmSIQ4RUMfeveA1KZbSf999NE4qKK8Do+8oXcmTM4LZVmh1rlyqznIdFXPN7x3pD4E0gb6/y69xtWMChv9654FMg05bAdueKt9uA4BEcAbpkdHF'
         | let input = base64'LcMT3OOlkHLzJBKCKjjzzVMg+r+FVgd52LlhZPB4RFg='
         | groth16Verify(key, proof, input)
       """.stripMargin
    eval(src, version = V4) shouldBe Right(CONST_BOOLEAN(true))
  }

  val grothsFail = Seq(
    (
      0,
      """
        |let vk = base64'lp7+dPDIOfm77haSFnvr33VwYH/KbIalfOJPRvBLzqlHD8BxunNebMr6Gr6S+u+nh7yLzdqr7HHQNOpZI8mdj/7lR0IBqB9zvRfyTr+guUG22kZo4y2KINDp272xGglKEeTglTxyDUriZJNF/+T6F8w70MR/rV+flvuo6EJ0+HA+A2ZnBbTjOIl9wjisBV+0iISo2JdNY1vPXlpwhlL2fVpW/WlREkF0bKlBadDIbNJBgM4niJGuEZDru3wqrGueETKHPv7hQ8em+p6vQolp7c0iknjXrGnvlpf4QtUtpg3z/D+snWjRPbVqRgKXWtihuIvPFaM6dt7HZEbkeMnXWwSINeYC/j3lqYnce8Jq+XkuF42stVNiooI+TuXECnFdFi9Ib25b9wtyz3H/oKg48He1ftntj5uIRCOBvzkFHGUF6Ty214v3JYvXJjdS4uS2jekplZYoV0aXEnYEOIvfF7d4xay3qkx2NspooM4HeZpiHknIWkUVhGVJBzBDLjLB'
        |let proof = base64'jiGBK+TGHfH8Oadexhdet7ExyIWibSmamWQvffZkyl3WnMoVbTQ3lOks4Mca3sU5qgcaLyQQ1FjFW4g6vtoMapZ43hTGKaWO7bQHsOCvdwHCdwJDulVH16cMTyS9F0BfBJxa88F+JKZc4qMTJjQhspmq755SrKhN9Jf+7uPUhgB4hJTSrmlOkTatgW+/HAf5kZKhv2oRK5p5kS4sU48oqlG1azhMtcHEXDQdcwf9ANel4Z9cb+MQyp2RzI/3hlIx'
        |let inputs = base64''
            """.stripMargin
    ),
    (
      1,
      """
        |let vk = base64'lp7+dPDIOfm77haSFnvr33VwYH/KbIalfOJPRvBLzqlHD8BxunNebMr6Gr6S+u+nh7yLzdqr7HHQNOpZI8mdj/7lR0IBqB9zvRfyTr+guUG22kZo4y2KINDp272xGglKEeTglTxyDUriZJNF/+T6F8w70MR/rV+flvuo6EJ0+HA+A2ZnBbTjOIl9wjisBV+0iISo2JdNY1vPXlpwhlL2fVpW/WlREkF0bKlBadDIbNJBgM4niJGuEZDru3wqrGueETKHPv7hQ8em+p6vQolp7c0iknjXrGnvlpf4QtUtpg3z/D+snWjRPbVqRgKXWtihuIvPFaM6dt7HZEbkeMnXWwSINeYC/j3lqYnce8Jq+XkuF42stVNiooI+TuXECnFdFi9Ib25b9wtyz3H/oKg48He1ftntj5uIRCOBvzkFHGUF6Ty214v3JYvXJjdS4uS2jekplZYoV0aXEnYEOIvfF7d4xay3qkx2NspooM4HeZpiHknIWkUVhGVJBzBDLjLBjiGBK+TGHfH8Oadexhdet7ExyIWibSmamWQvffZkyl3WnMoVbTQ3lOks4Mca3sU5'
        |let proof = base64'hp1iMepdu0rKoBh0NXcw9F9hkiggDIkRNINq2rlvUypPiSmp8U8tDSMeG0YVSovFteecr3THhBJj0qNeEe9jA2Ci64fKG9WT1heMYzEAQKebOErYXYCm9d72n97mYn1XBq+g1Y730XEDv4BIDI1hBDntJcgcj/cSvcILB1+60axJvtyMyuizxUr1JUBUq9njtmJ9m8zK6QZLNqMiKh0f2jokQb5mVhu6v5guW3KIjwQc/oFK/l5ehKAOPKUUggNh'
        |let inputs = base64'c9BSUPtO0xjPxWVNkEMfXe7O4UZKpaH/nLIyQJj7iA4='
            """.stripMargin
    ),
    (
      15,
      """
        |let vk = base64'lp7+dPDIOfm77haSFnvr33VwYH/KbIalfOJPRvBLzqlHD8BxunNebMr6Gr6S+u+nh7yLzdqr7HHQNOpZI8mdj/7lR0IBqB9zvRfyTr+guUG22kZo4y2KINDp272xGglKEeTglTxyDUriZJNF/+T6F8w70MR/rV+flvuo6EJ0+HA+A2ZnBbTjOIl9wjisBV+0iISo2JdNY1vPXlpwhlL2fVpW/WlREkF0bKlBadDIbNJBgM4niJGuEZDru3wqrGueETKHPv7hQ8em+p6vQolp7c0iknjXrGnvlpf4QtUtpg3z/D+snWjRPbVqRgKXWtihuIvPFaM6dt7HZEbkeMnXWwSINeYC/j3lqYnce8Jq+XkuF42stVNiooI+TuXECnFdFi9Ib25b9wtyz3H/oKg48He1ftntj5uIRCOBvzkFHGUF6Ty214v3JYvXJjdS4uS2jekplZYoV0aXEnYEOIvfF7d4xay3qkx2NspooM4HeZpiHknIWkUVhGVJBzBDLjLBjiGBK+TGHfH8Oadexhdet7ExyIWibSmamWQvffZkyl3WnMoVbTQ3lOks4Mca3sU5hp1iMepdu0rKoBh0NXcw9F9hkiggDIkRNINq2rlvUypPiSmp8U8tDSMeG0YVSovFlA4DsjBwntJH45NgNbY/Rbu/hfe7QskTkBiTo2A+kmYSH75Uvf2UAXwBAT1PoE0sqtYndF2Kbthl6GylV3j9NIKtIzHd/GwleExuM7KlI1H22P78br5zmh8D7V1aFcxPpftQhjch4abXuxEP4ahgfNmthdhoSvQykLhjbmG9BrvwmyaDRd/sHCTeSXmLqIybrd6tA8ZLJq2DLzKJEOlmfM9aIihLe/FLndfnTSkNK2et4o8vM3YjAmgOnrAo7JIpl0Zot59NUiTdx5j27IV+8siRWRRz9U3vtvz421qgPE5kn6YrJSVnYKCoWeB3FNfph1V+Mh894o3SLdj9n7ogflH/sfXisYj5vleSNldJi/67TKM4BgI1aaGdXuTteHqKti66rXQ+9a9d+SmwKgnRUpjVu1tkrWZCSFbVuugZYEZ9BZjhVCSY636wBuG6KFv7sDKiiZ0vXRqpUjUCOFMfkTG9nJdoOtatjliAef7+DTX3tUTl1mVdNczmAnEgeiZJq3mMKxcbKicOXQscqU/Jgd1+Y2bsyQsDIgwN/k23y7jAuaEhIPlMeLzL84Jkl5N8sbAIh35qXZz7tesyYdt8FuJX6GCu6qXKOFs8aFn8RV2x9Ba8z5iHBCwS7QOCmZnakywU/Lb2kFEaqsA2K8W/3ZDw2tW5mNQqLlH/MRoGp4SMLs6a0CKO2Ph0532oePpDlgQoF1kX9pyf9UBQaNIfrkXDGQGS/r2y6LZTdPivYs6l9r6ARUxisRRzqbe8WvxVoPaJvr8Xg/dqQWz2lYgtCdiGWbjvNUhDYpKdzR+8v8IRerYlH6L8RppDRhiCzQTU'
        |let proof = base64'pNeWbxzzJPMsPpuXBXWZgtLic1s0KL8UeLDGBhEjygrv8m1eMM12pzd+r/scvBEHrnEoQHanlNTlWPywaXaFtB5Hd5RMrnbfLbpe16tvtlH2SRbJbGXSpib5uiuSa6z1ExLtXs9nNWiu10eupG6Pq4SNOacCEVvUgSzCzhyLIlz62gq4DlBBWKmEFI7KiFs7kr2EPBjj2m83dbA/GGVgoYYjgBmFX6/srvLADxerZTKG2moOQrmAx9GJ99nwhRbW'
        |let inputs = base64'I8C5RcBDPi2n4omt9oOV2rZk9T9xlSV8PQvLeVHjGb00fCVz7AHOIjLJ03ZCTLQwEKkAk9tQWJ6gFTBnG2+0DDHlXcVkwpMafcpS2diKFe0T4fRb0t9mxNzOFiRVcJoeMU1zb/rE4dIMm9rbEPSDnVSOd8tHNnJDkT+/NcNsQ2w0UEVJJRAEnC7G0Y3522RlDLxpTZ6w0U/9V0pLNkFgDCkFBKvpaEfPDJjoEVyCUWDC1ts9LIR43xh3ZZBdcO/HATHoLzxM3Ef11qF+riV7WDPEJfK11u8WGazzCAFhsx0aKkkbnKl7LnypBzwRvrG2JxdLI/oXL0eoIw9woVjqrg6elHudnHDXezDVXjRWMPaU+L3tOW9aqN+OdP4AhtpgT2CoRCjrOIU3MCFqsrCK9bh33PW1gtNeHC78mIetQM5LWZHtw4KNwafTrQ+GCKPelJhiC2x7ygBtat5rtBsJAVF5wjssLPZx/7fqNqifXB7WyMV7J1M8LBQVXj5kLoS9bpmNHlERRSadC0DEUbY9xhIG2xo7R88R0sq04a299MFv8XJNd+IdueYiMiGF5broHD4UUhPxRBlBO3lOfDTPnRSUGS3Sr6GxwCjKO3MObz/6RNxCk9SnQ4NccD17hS/m'
            """.stripMargin
    ),
    (
      16,
      """
        |let vk = base64'lp7+dPDIOfm77haSFnvr33VwYH/KbIalfOJPRvBLzqlHD8BxunNebMr6Gr6S+u+nh7yLzdqr7HHQNOpZI8mdj/7lR0IBqB9zvRfyTr+guUG22kZo4y2KINDp272xGglKEeTglTxyDUriZJNF/+T6F8w70MR/rV+flvuo6EJ0+HA+A2ZnBbTjOIl9wjisBV+0iISo2JdNY1vPXlpwhlL2fVpW/WlREkF0bKlBadDIbNJBgM4niJGuEZDru3wqrGueETKHPv7hQ8em+p6vQolp7c0iknjXrGnvlpf4QtUtpg3z/D+snWjRPbVqRgKXWtihuIvPFaM6dt7HZEbkeMnXWwSINeYC/j3lqYnce8Jq+XkuF42stVNiooI+TuXECnFdFi9Ib25b9wtyz3H/oKg48He1ftntj5uIRCOBvzkFHGUF6Ty214v3JYvXJjdS4uS2jekplZYoV0aXEnYEOIvfF7d4xay3qkx2NspooM4HeZpiHknIWkUVhGVJBzBDLjLBjiGBK+TGHfH8Oadexhdet7ExyIWibSmamWQvffZkyl3WnMoVbTQ3lOks4Mca3sU5hp1iMepdu0rKoBh0NXcw9F9hkiggDIkRNINq2rlvUypPiSmp8U8tDSMeG0YVSovFlA4DsjBwntJH45NgNbY/Rbu/hfe7QskTkBiTo2A+kmYSH75Uvf2UAXwBAT1PoE0sqtYndF2Kbthl6GylV3j9NIKtIzHd/GwleExuM7KlI1H22P78br5zmh8D7V1aFcxPpftQhjch4abXuxEP4ahgfNmthdhoSvQykLhjbmG9BrvwmyaDRd/sHCTeSXmLqIybrd6tA8ZLJq2DLzKJEOlmfM9aIihLe/FLndfnTSkNK2et4o8vM3YjAmgOnrAo7JIpl0Zot59NUiTdx5j27IV+8siRWRRz9U3vtvz421qgPE5kn6YrJSVnYKCoWeB3FNfph1V+Mh894o3SLdj9n7ogflH/sfXisYj5vleSNldJi/67TKM4BgI1aaGdXuTteHqKti66rXQ+9a9d+SmwKgnRUpjVu1tkrWZCSFbVuugZYEZ9BZjhVCSY636wBuG6KFv7sDKiiZ0vXRqpUjUCOFMfkTG9nJdoOtatjliAef7+DTX3tUTl1mVdNczmAnEgeiZJq3mMKxcbKicOXQscqU/Jgd1+Y2bsyQsDIgwN/k23y7jAuaEhIPlMeLzL84Jkl5N8sbAIh35qXZz7tesyYdt8FuJX6GCu6qXKOFs8aFn8RV2x9Ba8z5iHBCwS7QOCmZnakywU/Lb2kFEaqsA2K8W/3ZDw2tW5mNQqLlH/MRoGp4SMLs6a0CKO2Ph0532oePpDlgQoF1kX9pyf9UBQaNIfrkXDGQGS/r2y6LZTdPivYs6l9r6ARUxisRRzqbe8WvxVoPaJvr8Xg/dqQWz2lYgtCdiGWbjvNUhDYpKdzR+8v8IRerYlH6L8RppDRhiCzQTUpNeWbxzzJPMsPpuXBXWZgtLic1s0KL8UeLDGBhEjygrv8m1eMM12pzd+r/scvBEH'
        |let proof = base64'iw5yhCCarVRq/h0Klq4tHNdF1j7PxaDn0AfHTxc2hb//Acav53QStwQShQ0BpQJ7sdchkTTJLkhM13+JpPY/I2WIc6DMZdRzw3pRjLSdMUmce7LYbBJOI+/IyuLZH5IXA7sX4r+xrPssIaMiKR3twmmReN9NrSoovLepDsNmzDVraO71B4rkx7uPXvkqvt3Zkr2EPBjj2m83dbA/GGVgoYYjgBmFX6/srvLADxerZTKG2moOQrmAx9GJ99nwhRbW'
        |let inputs = base64'I8C5RcBDPi2n4omt9oOV2rZk9T9xlSV8PQvLeVHjGb00fCVz7AHOIjLJ03ZCTLQwEKkAk9tQWJ6gFTBnG2+0DDHlXcVkwpMafcpS2diKFe0T4fRb0t9mxNzOFiRVcJoeMU1zb/rE4dIMm9rbEPSDnVSOd8tHNnJDkT+/NcNsQ2w0UEVJJRAEnC7G0Y3522RlDLxpTZ6w0U/9V0pLNkFgDCkFBKvpaEfPDJjoEVyCUWDC1ts9LIR43xh3ZZBdcO/HATHoLzxM3Ef11qF+riV7WDPEJfK11u8WGazzCAFhsx0aKkkbnKl7LnypBzwRvrG2JxdLI/oXL0eoIw9woVjqrg6elHudnHDXezDVXjRWMPaU+L3tOW9aqN+OdP4AhtpgT2CoRCjrOIU3MCFqsrCK9bh33PW1gtNeHC78mIetQM5LWZHtw4KNwafTrQ+GCKPelJhiC2x7ygBtat5rtBsJAVF5wjssLPZx/7fqNqifXB7WyMV7J1M8LBQVXj5kLoS9bpmNHlERRSadC0DEUbY9xhIG2xo7R88R0sq04a299MFv8XJNd+IdueYiMiGF5broHD4UUhPxRBlBO3lOfDTPnRSUGS3Sr6GxwCjKO3MObz/6RNxCk9SnQ4NccD17hS/mEFt8d4ERZOfmuvD3A0RCPCnx3Fr6rHdm6j+cfn/NM6o='
            """.stripMargin
    )
  )

  property("groth16Verify_*inputs (false)") {
    for ((ii, lets) <- grothsFail) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  val grothsOk = Seq(
    (
      0,
      """
        |let vk = base64'kYYCAS8vM2T99GeCr4toQ+iQzvl5fI89mPrncYqx3C1d75BQbFk8LMtcnLWwntd6knkzSwcsialcheg69eZYPK8EzKRVI5FrRHKi8rgB+R5jyPV70ejmYEx1neTmfYKODRmARr/ld6pZTzBWYDfrCkiS1QB+3q3M08OQgYcLzs/vjW4epetDCmk0K1CEGcWdh7yLzdqr7HHQNOpZI8mdj/7lR0IBqB9zvRfyTr+guUG22kZo4y2KINDp272xGglKEeTglTxyDUriZJNF/+T6F8w70MR/rV+flvuo6EJ0+HA+A2ZnBbTjOIl9wjisBV+0jgld4oAppAOzvQ7eoIx2tbuuKVSdbJm65KDxl/T+boaYnjRm3omdETYnYRk3HAhrAeWpefX+dM/k7PrcheInnxHUyjzSzqlN03xYjg28kdda9FZJaVsQKqdEJ/St9ivXlp7+dPDIOfm77haSFnvr33VwYH/KbIalfOJPRvBLzqlHD8BxunNebMr6Gr6S+u+n'
        |let proof = base64'sStVLdyxqInmv76iaNnRFB464lGq48iVeqYWSi2linE9DST0fTNhxSnvSXAoPpt8tFsanj5vPafC+ij/Fh98dOUlMbO42bf280pOZ4lm+zr63AWUpOOIugST+S6pq9zeB0OHp2NY8XFmriOEKhxeabhuV89ljqCDjlhXBeNZwM5zti4zg89Hd8TbKcw46jAsjIJe2Siw3Th7ELQQKR5ucX50f0GISmnOSceePPdvjbGJ8fSFOnSmSp8dK7uyehrU'
        |let inputs = base64''
        |""".stripMargin
    ),
    (
      1,
      """
        |let vk = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
        |let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
        |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs='
        |""".stripMargin
    ),
    (
      15,
      """
        |let vk = base64'tRpqHB4HADuHAUvHTcrzxmq1awdwEBA0GOJfebYTODyUqXBQ7FkYrz1oDvPyx5Z3sUmODSJXAQmAFBVnS2t+Xzf5ZCr1gCtMiJVjQ48/nob/SkrS4cTHHjbKIVS9cdD/BG/VDrZvBt/dPqXmdUFyFuTTMrViagR57YRrDmm1qm5LQ/A8VwUBdiArwgRQXH9jsYhgVmfcRAjJytrbYeR6ck4ZfmGr6x6akKiBLY4B1l9LaHTyz/6KSM5t8atpuR3HBJZfbBm2/K8nnYTl+mAU/EnIN3YQdUd65Hsd4Gtf6VT2qfz6hcrSgHutxR1usIL2kyU9X4Kqjx6I6zYwVbn7PWbiy3OtY277z4ggIqW6AuDgzUeIyG9a4stMeQ07mOV/Ef4faj+eh4GJRKjJm7aUTYJCSAGY6klOXNoEzB54XF4EY5pkMPfW73SmxJi9B0aHkZWDy2tzUlwvxZ/BfsDkUZnt6mI+qdDOtTG6JFItSQZotYGDBm6zPczwo3ZAGpr8gibTE6DjT7GGNDEl26jgAJ3aAdBrf7Yb0vWEYizOJK4SO/Ud+4/WxXDby7xbwlFYkgEtYbMO6PXozhRqDiotJ0CfdSExNHA9A37mR/bpNOKyhArfyvSBIJnUQgOw5wMBq+GOP5n78E99a5rY4FXGUmM3LGdp/CvkGITYf04SWHkZAEueYH96Ys5jrHlIZQA2k9j02Ji+SL82DJFH8LDh77fgh9zh0wAjCAqY7/r72434RDA97bfEZJavRmAENsgflsSVb8d9rQMBpWl3Xkb8mNlUOSf+LAXeXYQR42Z4yuUjwAUvk//+imuhsWF8ZCMkpb9wQ/6crVH4E5E3f6If/Mt/DcenWlPNtvu2CJFatc8q31aSdnWhMN8U65SX3DBouDc8EXDFd5twy4VWMS5lhY6VbU/lS8T8oyhr+NIpstsKUmSh0EM1rGyUh2PNgIYzoeBznHWagp2WO3nIbNYIcXEROBT8QpqA4Dqzxv665jwajGXmAawRvdZqzLqvCkeujekplZYoV0aXEnYEOIvfF7d4xay3qkx2NspooM4HeZpiHknIWkUVhGVJBzBDLjLBjiGBK+TGHfH8Oadexhdet7ExyIWibSmamWQvffZkyl3WnMoVbTQ3lOks4Mca3sU5hp1iMepdu0rKoBh0NXcw9F9hkiggDIkRNINq2rlvUypPiSmp8U8tDSMeG0YVSovFlA4DsjBwntJH45NgNbY/Rbu/hfe7QskTkBiTo2A+kmYSH75Uvf2UAXwBAT1PoE0sqtYndF2Kbthl6GylV3j9NIKtIzHd/GwleExuM7KlI1H22P78br5zmh8D7V1aFcxPpftQhjch4abXuxEP4ahgfNmthdhoSvQykLhjbmG9BrvwmyaDRd/sHCTeSXmLqIybrd6tA8ZLJq2DLzKJEOlmfM9aIihLe/FLndfnTSkNK2et4o8vM3YjAmgOnrAo7JIp'
        |let proof = base64'lgFU4Jyo9GdHL7w31u3zXc8RQRnHVarZWNfd0lD45GvvQtwrZ1Y1OKB4T29a79UagPHOdk1S0k0hYAYQyyNAfRUzde1HP8R+2dms75gGZEnx2tXexEN+BVjRJfC8PR1lFJa6xvsEx5uSrOZzKmoMfCwcA55SMT5jFo4+KyWg2wP5OnFPx7XTdEKvf5YhpY0krQKiq3OUu79EwjNF1xV1+iLxx2KEIyK7RSYxO1BHrKOGOEzxSUK00MA+YVHe+DvW'
        |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEtiLNj7hflFeVnNXPguxyoqkI/V7pGJtXBpH5N+RswQNA0b23aM33aH0HKHOWoGY/T/L7TQzYFGJ3vTLiXDFZg1OVqkGOMvqAgonOrHGi6IgcALyUMyCKlL5BQY23SeILJpYKolybJNwJfbjxpg0Oz+D2fr7r9XL1GMvgblu52bVQT1fR8uCRJfSsgA2OGw6k/MpKDCfMcjbR8jnZa8ROEvF4cohm7iV1788Vp2/2bdcEZRQSoaGV8pOmA9EkqzJVRABjkDso40fnQcm2IzjBUOsX+uFExVan56/vl9VZVwB0wnee3Uxiredn0kOayiPB16yimxXCDet+M+0UKjmIlmXYpkrCDrH0dn53w+U3OHqMQxPDnUpYBxadM1eI8xWFFxzaLkvega0q0DmEquyY02yiTqo+7Q4qaJVTLgu6/8ekzPxGKRi845NL8gRgaTtM3kidDzIQpyODZD0yeEZDY1M+3sUKHcVkhoxTQBTMyKJPc+M5DeBL3uaWMrvxuL6q8+X0xeBt+9kguPUNtIYqUgPAaXvM2i041bWHTJ0dZLyDJVOyzGaXRaF4mNkAuh4Et6Zw5PuOpMM2mI1oFKEZj7'
        |""".stripMargin
    ),
    (
      16,
      """
        |let vk = base64'kY4NWaOoYItWtLKVQnxDh+XTsa0Yev5Ae3Q9vlQSKp6+IUtwS7GH5ZrZefmBEwWEqvAtYaSs5qW3riOiiRFoLp7MThW4vCEhK0j8BZY5ZM/tnjB7mrLB59kGvzpW8PM/AoQRIWzyvO3Dxxfyj/UQcQRw+KakVRvrFca3Vy2K5cFwxYHwl6PFDM+OmGrlgOCoqZtY1SLOd+ovmFOODKiHBZzDZhC/lRfjKVy4LzI7AXDuFn4tlWoT7IsJyy6lYNaWFfLjYZPAsrv1gXJ1NYat5B6E0Pnz5C67u2Uigmlol2D91re3oAqIo+r8kiyFKOSBooG0cMN47zQor6qj0owuxJjn5Ymrcd/FCQ1ud4cKoUlNaGWIekSjxJEB87elMy5oEUlUzVI9ObMm+2SE3Udgws7pkMM8fgQUQUqUVyc7sNCE9m/hQzlwtbXrNSS5Pb+6ow7aHMOavjVyaXiS0f6b1pwJpS1yT+K85UA1CLqqxCaEw5+8WAjMzBOrKmxBUpYApI4FBAIa/SjeU/wYnljUUMTMfnBfCQ8MS01hFSQZSoPx1do8Zxn5Y3NPgpaomXDfpyVK9Q0U0NkqQqPsk+T+AroxQGxq9f/HOX5I5ZibF27dZ32tCbTKo22GgspqtAv2iv06PubySY5lRIEYlCjr5j8Ahl9gFvN+22cIh1iGiuwByhPjGDgP5h78xZXCBoJekEYPcI2C0LtBch5pZC/JpS1kF9lBLndodhIlutEr3mkKohR+D/czN/FTdxU2b82QqfZOHc+6rv2biEXy8AdoAMykj1dsIw7/d5M8XcgPiUzNko4H6p02Rt2R01MOYboTogaQH8lyU6o8c+iORRGEoZDTq4htC+Qa7AXTodvSmG33IrwJVGOKDMtvWI1VYdhWs32SB0W1d+BrFb0ObBGsz+Un7P+V8qerCMqu906BkbjdWmsKbKQBFC8/YDTdSi92rIq1ISUQWn88AgW/q+u6KPxybU5EZgbA+EZwCDB6MyBNhHcrAvVFeX+kj1RY1Gx1kzCE3ldsT37sCbayFtyMMbL6gDQCoTadJX/jhs9wgp0dZujwOk0Wefhgy1BUHXl/q+2nXAKPvKmli6Wo7/pYr/q13Gcsj7Z7WSKVn4Fm4XfkJD62q6paCxO51BlJQEcnpNPKS7+zjhmQlTRiEryD8ve7KQzk20eb4TgIMR1hI5pnQmjGeT56xZySp2nDnYDsqsnXB5uQY8lyf6IYC/PHzEb3rSx91k0ZEu5w5IMrVK8otNzZHrUuM0aPdImpLQJ4qEgvmezORpcUCq4SRp9bGl3/yzXE5tWZgn3Q6kXyjFMhu+foTYy1NV+HJbJI1nYMjeTr3f+RxSphIYWyMZ7sD3RgDzRk5iQqD1J+8rdOIZliObfrmWaro/BBxNvd1fPAlFEPiDegBcDaVWHS2A1FPIC9d+DU05vizrBfli6su9rCvSBNVnoDSBF2zeU+2NjXj7ycHYxCuZgl8dBu8FZjvjlDUZCqfdq3PszQeo2X55trDJEHeVWaRoIcgiG2hfTN'
        |let proof = base64'jqPSA/XKqZDJnRSmM0sJxbrFv7GUcA45QMysIx1xTsI3+2iysF5Tr68565ZuO65qjo2lklZpQo+wtyKSA/56EaKOJZCZhSvDdBEdvVYJCjmWusuK5qav7xZO0w5W1qRiEgIdcGUz5V7JHqfRf4xI6/uUD846alyzzNjxQtKErqJbRw6yyBO6j6box363pinjiMTzU4w/qltzFuOEpKxy/H3vyH8RcsF24Ou/Rb6vfR7cSLtLwCsf/BMtPcsQfdRK'
        |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEtiLNj7hflFeVnNXPguxyoqkI/V7pGJtXBpH5N+RswQNA0b23aM33aH0HKHOWoGY/T/L7TQzYFGJ3vTLiXDFZg1OVqkGOMvqAgonOrHGi6IgcALyUMyCKlL5BQY23SeILJpYKolybJNwJfbjxpg0Oz+D2fr7r9XL1GMvgblu52bVQT1fR8uCRJfSsgA2OGw6k/MpKDCfMcjbR8jnZa8ROEvF4cohm7iV1788Vp2/2bdcEZRQSoaGV8pOmA9EkqzJVRABjkDso40fnQcm2IzjBUOsX+uFExVan56/vl9VZVwB0wnee3Uxiredn0kOayiPB16yimxXCDet+M+0UKjmIlmXYpkrCDrH0dn53w+U3OHqMQxPDnUpYBxadM1eI8xWFFxzaLkvega0q0DmEquyY02yiTqo+7Q4qaJVTLgu6/8ekzPxGKRi845NL8gRgaTtM3kidDzIQpyODZD0yeEZDY1M+3sUKHcVkhoxTQBTMyKJPc+M5DeBL3uaWMrvxuL6q8+X0xeBt+9kguPUNtIYqUgPAaXvM2i041bWHTJ0dZLyDJVOyzGaXRaF4mNkAuh4Et6Zw5PuOpMM2mI1oFKEZj7Xqf/yAmy/Le3GfJnMg5vNgE7QxmVsjuKUP28iN8rdi4='
        |""".stripMargin
    ),
    (
      17,
      """
        |let vk = base64'pQUlLSBu9HmVa9hB0rEu1weeBv2RKQQ8yCHpwXTHeSkcQqmSOuzednF8o0+MdyNuhKgxmPN2c94UBtlYc0kZS6CwyMEEV/nVGSjajEZPdnpbK7fEcPd0hWNcOxKWq8qBBPfT69Ore74buf8C26ZTyKnjgMsGCvoDAMOsA07DjjQ1nIkkwIGFFUT3iMO83TdEpWgV/2z7WT9axNH/QFPOjXvwQJFnC7hLxHnX6pgKOdAaioKdi6FX3Y2SwWEO3UuxFd3KwsrZ2+mma/W3KP/cPpSzqyHa5VaJwOCw6vSM4wHSGKmDF4TSrrnMxzIYiTbTlrwLi5GjMxD6BKzMMN9+7xFuO7txLCEIhGrIMFIvqTw1QFAO4rmAgyG+ljlYTfWHAkzqvImL1o8dMHhGOTsMLLMg39KsZVqalZwwL3ckpdAf81OJJeWCpCuaSgSXnWhJmHxQuA9zUhrmlR1wHO9eegHh/p01osP0xU03rY1oGonOZ28acYG6MSOfZBkKT+NoqOcEWtL4RCP6t7BWXHgIUmlhCEj/pwNVx92Vc3ZzE8zMh3U196ICHzTSZz0rMwJkmT0l1m7QdvBpqUeqCxyXgY+6afqsdAdGjZeuUOPB2RDam3Cm2j2Z5VygvdIBI12qlIoEBhnrhCxx6TN+ywilfI2aBjzTtn0rCe7IA9sYtcYn3XSooU7TBNB39O8cbGgnmGYQygxBsQ/Emj2KDCqQ4A1MRnSe3q6tQhjToqDjHRXEKzlWka/4+hWNnJpicq/LmT3jxCH9/yre8qFUXy+Hq2ycitjv3rogw+hyXlK3pIoQmDskJnqBk3hxisj3QQrQiv06PubySY5lRIEYlCjr5j8Ahl9gFvN+22cIh1iGiuwByhPjGDgP5h78xZXCBoJekEYPcI2C0LtBch5pZC/JpS1kF9lBLndodhIlutEr3mkKohR+D/czN/FTdxU2b82QqfZOHc+6rv2biEXy8AdoAMykj1dsIw7/d5M8XcgPiUzNko4H6p02Rt2R01MOYboTogaQH8lyU6o8c+iORRGEoZDTq4htC+Qa7AXTodvSmG33IrwJVGOKDMtvWI1VYdhWs32SB0W1d+BrFb0ObBGsz+Un7P+V8qerCMqu906BkbjdWmsKbKQBFC8/YDTdSi92rIq1ISUQWn88AgW/q+u6KPxybU5EZgbA+EZwCDB6MyBNhHcrAvVFeX+kj1RY1Gx1kzCE3ldsT37sCbayFtyMMbL6gDQCoTadJX/jhs9wgp0dZujwOk0Wefhgy1BUHXl/q+2nXAKPvKmli6Wo7/pYr/q13Gcsj7Z7WSKVn4Fm4XfkJD62q6paCxO51BlJQEcnpNPKS7+zjhmQlTRiEryD8ve7KQzk20eb4TgIMR1hI5pnQmjGeT56xZySp2nDnYDsqsnXB5uQY8lyf6IYC/PHzEb3rSx91k0ZEu5w5IMrVK8otNzZHrUuM0aPdImpLQJ4qEgvmezORpcUCq4SRp9bGl3/yzXE5tWZgn3Q6kXyjFMhu+foTYy1NV+HJbJI1nYMjeTr3f+RxSphIYWyMZ7sD3RgDzRk5iQqD1J+8rdOIZliObfrmWaro/BBxNvd1fPA'
        |let proof = base64'qV2FNaBFqWeL6n9q9OUbCSTcIQvwO0vfaA/f/SxEtLSIaOGIOx8r+WVGFdxmC6i3oOaoEkJWvML7PpKBDtqiK7pKDIaMV5PkV/kQl6UgxZv9OInTwpVPtYcgeeTokG/eBi1qKzJwDoEHVqKeLqrLXJHXhBVQLdoIUOeKj8YMkagVniO9EtK0fW0/9QnRIxXoilxSj5HBEpYwFBitJXRk1ftFGWZFxJXU5PXdRmC+pomyo5Scx+UJQ2NLRWHjKlV0'
        |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEtiLNj7hflFeVnNXPguxyoqkI/V7pGJtXBpH5N+RswQNA0b23aM33aH0HKHOWoGY/T/L7TQzYFGJ3vTLiXDFZg1OVqkGOMvqAgonOrHGi6IgcALyUMyCKlL5BQY23SeILJpYKolybJNwJfbjxpg0Oz+D2fr7r9XL1GMvgblu52bVQT1fR8uCRJfSsgA2OGw6k/MpKDCfMcjbR8jnZa8ROEvF4cohm7iV1788Vp2/2bdcEZRQSoaGV8pOmA9EkqzJVRABjkDso40fnQcm2IzjBUOsX+uFExVan56/vl9VZVwB0wnee3Uxiredn0kOayiPB16yimxXCDet+M+0UKjmIlmXYpkrCDrH0dn53w+U3OHqMQxPDnUpYBxadM1eI8xWFFxzaLkvega0q0DmEquyY02yiTqo+7Q4qaJVTLgu6/8ekzPxGKRi845NL8gRgaTtM3kidDzIQpyODZD0yeEZDY1M+3sUKHcVkhoxTQBTMyKJPc+M5DeBL3uaWMrvxuL6q8+X0xeBt+9kguPUNtIYqUgPAaXvM2i041bWHTJ0dZLyDJVOyzGaXRaF4mNkAuh4Et6Zw5PuOpMM2mI1oFKEZj7Xqf/yAmy/Le3GfJnMg5vNgE7QxmVsjuKUP28iN8rdi4bUp7c0KJpqLXE6evfRrdZBDRYp+rmOLLDg55ggNuwog=='
        |""".stripMargin
    )
  )

  property("groth16Verify_*inputs") {
    for ((ii, lets) <- grothsOk if (ii <= 16)) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    }
  }

  property("groth16Verify_*inputs fail if too many inputs") {
    for ((i, lets) <- grothsFail ++ grothsOk if (i > 1)) {
      val src = lets ++ (if (i <= 16) {
                           s"groth16Verify_${i - 1}inputs(vk, proof, inputs)"
                         } else {
                           "groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Left(s"Invalid inputs size ${i * 32} bytes, must be not greater than ${i * 32 - 32} bytes")
    }
  }

  property("groth16Verify_*inputs with invalid vk size") {
    for {
      ii <- 0 to 16
      n  <- Seq(0, (8 + ii) * 48 + 1, (8 + ii) * 48 - 1)
    } {
      val lets = s"""
                    |let vk = base16'${"AA" * n}'
                    |let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
                    |let inputs = base16'${"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" * ii}'
                    |""".stripMargin
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Left(s"Invalid vk size ${n} bytes, must be equal to ${(8 + ii) * 48} bytes for ${ii} inputs")
    }
  }

  property("groth16Verify_*inputs with invalid proof size") {
    val lets =
      """
        |let vk = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
        |let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d' + base16'00'
        |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs='
        |""".stripMargin
    for (ii <- 0 to 16) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Left("Invalid proof size 193 bytes, must be equal to 192 bytes")
    }
  }

  property("groth16Verify_*inputs with invalid inputs size") {
    val lets =
      """
        |let vk = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
        |let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
        |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs=' + base16'00'
        |""".stripMargin
    for (ii <- 2 to 16) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Left("Invalid inputs size 33 bytes, must be a multiple of 32 bytes")
    }
  }

  property("calculateAssetId") {
    val decimals     = 100
    val description  = "description"
    val isReissuable = true
    val name         = "name"
    val quantity     = 1234567
    val nonce        = 1
    val issue        = Issue.create(compiledScript = None, decimals, description, isReissuable, name, quantity, nonce, ByteStr.empty)
    val script =
      s"""
         | let issue = Issue("$name", "$description", $quantity, $decimals, $isReissuable, unit, $nonce)
         | calculateAssetId(issue)
      """.stripMargin

    val ctx = WavesContext.build(Global, DirectiveSet(V4, Account, DApp).explicitGet(), fixBigScriptField = true)

    genericEval[Environment, EVALUATED](script, ctxt = ctx, version = V4, env = utils.environment) shouldBe
      CONST_BYTESTR(issue.id)
  }

  property("different Issue action constructors") {
    val script =
      """
        | Issue("name", "description", 1234567, 100, true) ==
        | Issue("name", "description", 1234567, 100, true, unit, 0)
     """.stripMargin

    val ctx = WavesContext.build(Global, DirectiveSet(V4, Account, DApp).explicitGet(), fixBigScriptField = true)

    genericEval[Environment, EVALUATED](script, ctxt = ctx, version = V4, env = utils.environment) shouldBe
      Right(CONST_BOOLEAN(true))
  }

  property("toBase16String limit 8Kb from V4") {
    val base16String8Kb              = "fedcba9876543210" * 1024
    def script(base16String: String) = s"toBase16String(base16'$base16String')"

    eval(script(base16String8Kb), version = V3) shouldBe CONST_STRING(base16String8Kb)
    eval(script(base16String8Kb), version = V4) shouldBe CONST_STRING(base16String8Kb)

    eval(script(base16String8Kb + "aa"), version = V3) shouldBe CONST_STRING(base16String8Kb + "aa")
    eval(script(base16String8Kb + "aa"), version = V4) shouldBe Left("Base16 encode input length=8193 should not exceed 8192")
  }

  property("fromBase16String supports mixed case input") {
    def bytes(base16String: String) = ByteStr(BaseEncoding.base16().decode(base16String.toUpperCase))

    val value  = "fedcba9876543210FEDCBA9876543210"
    val script = s"""fromBase16String("$value")"""

    eval(script) shouldBe CONST_BYTESTR(bytes(value.toUpperCase))
  }

  property("string limit") {
    val almostMaxString               = "a" * (Terms.DataEntryValueMax - 1)
    val maxBytes                      = ("a" * Terms.DataEntryValueMax).getBytes(StandardCharsets.UTF_8)
    val constructingTooBigString      = s""" "$almostMaxString" + "aa" """
    val constructingMaxStringAndBytes = s""" ("$almostMaxString" + "a").toBytes() """

    eval(constructingMaxStringAndBytes, version = V3) shouldBe CONST_BYTESTR(ByteStr(maxBytes))
    eval(constructingMaxStringAndBytes, version = V4) shouldBe CONST_BYTESTR(ByteStr(maxBytes))

    eval(constructingTooBigString, version = V3) should produce("String size = 32768 exceeds 32767 bytes")
    eval(constructingTooBigString, version = V4) should produce("String size = 32768 exceeds 32767 bytes")
  }

  property("bytes limit") {
    val bytes = ByteStr(("a" * (Terms.DataEntryValueMax / 4)).getBytes(StandardCharsets.UTF_8))
    val constructingMaxBytes =
      s""" base64'${bytes.base64Raw}' +
         | base64'${bytes.base64Raw}' +
         | base64'${bytes.base64Raw}' +
         | base64'${bytes.base64Raw}' +
         | base58'aaa'
       """.stripMargin
    val constructingTooBigBytes = constructingMaxBytes + "+ base58'a'"

    inside(eval[EVALUATED](constructingMaxBytes, version = V3)) { case Right(CONST_BYTESTR(bytes)) =>
      bytes.size shouldBe Terms.DataEntryValueMax
    }

    inside(eval[EVALUATED](constructingMaxBytes, version = V4)) { case Right(CONST_BYTESTR(bytes)) =>
      bytes.size shouldBe Terms.DataEntryValueMax
    }

    eval(constructingTooBigBytes, version = V3) should produce("ByteVector size = 32768 bytes exceeds 32767")
    eval(constructingTooBigBytes, version = V4) should produce("ByteVector size = 32768 bytes exceeds 32767")
  }

  property("list indexOf") {
    eval(""" ["a","b","c","d"].indexOf("a") """, version = V4) shouldBe Right(CONST_LONG(0))
    eval(""" ["a","b","c","d","a"].indexOf("a") """, version = V4) shouldBe Right(CONST_LONG(0))
    eval(""" ["a","b","c","d"].indexOf("d") """, version = V4) shouldBe Right(CONST_LONG(3))
    eval(""" [-1,2,3,4].indexOf(-1) """, version = V4) shouldBe Right(CONST_LONG(0))
    eval(""" [-1,2,3,4].indexOf(4) """, version = V4) shouldBe Right(CONST_LONG(3))
    eval(""" [true, false].indexOf(true) """, version = V4) shouldBe Right(CONST_LONG(0))
    eval(""" [true, false].indexOf(false) """, version = V4) shouldBe Right(CONST_LONG(1))
    eval("""  [base58'a', base58'b', base58'c', base58'd'].indexOf(base58'a') """, version = V4) shouldBe Right(CONST_LONG(0))
    eval("""  [base58'a', base58'b', base58'c', base58'd'].indexOf(base58'd') """, version = V4) shouldBe Right(CONST_LONG(3))
    eval(""" ["a","b","c","d"].indexOf("e") """, version = V4) shouldBe Right(unit)

    eval(""" [true, false].indexOf(0) """, version = V4) should produce("Can't find a function overload")
    eval(""" [true, false].indexOf() """, version = V4) should produce("Can't find a function overload")
    eval(""" ["a","b","c","d"].indexOf("a") """, version = V3) should produce("Can't find a function overload 'indexOf'")
  }

  property("list lastIndexOf") {
    eval(""" ["a","b","a","c","d"].lastIndexOf("a") """, version = V4) shouldBe Right(CONST_LONG(2))
    eval(""" ["d","a","b","c","d"].lastIndexOf("d") """, version = V4) shouldBe Right(CONST_LONG(4))
    eval(""" [-1,2,3,4,-1].lastIndexOf(-1) """, version = V4) shouldBe Right(CONST_LONG(4))
    eval(""" [4,-1,2,3].lastIndexOf(4) """, version = V4) shouldBe Right(CONST_LONG(0))
    eval(""" [true, false].lastIndexOf(true) """, version = V4) shouldBe Right(CONST_LONG(0))
    eval(""" [true, false].lastIndexOf(false) """, version = V4) shouldBe Right(CONST_LONG(1))
    eval("""  [base58'a', base58'b', base58'c', base58'd'].lastIndexOf(base58'a') """, version = V4) shouldBe Right(CONST_LONG(0))
    eval("""  [base58'a', base58'b', base58'c', base58'd'].lastIndexOf(base58'd') """, version = V4) shouldBe Right(CONST_LONG(3))
    eval(""" ["a","b","c","d"].lastIndexOf("e") """, version = V4) shouldBe Right(unit)

    eval(""" [true, false].lastIndexOf(0) """, version = V4) should produce("Can't find a function overload")
    eval(""" [true, false].lastIndexOf() """, version = V4) should produce("Can't find a function overload")
    eval(""" ["a","b","c","d"].lastIndexOf("a") """, version = V3) should produce("Can't find a function overload 'lastIndexOf'")
  }

  property("list indexOf compare Limits") {
    val maxCmpWeightElement          = "a" * ContractLimits.MaxCmpWeight.toInt
    val maxSizeElementToFound        = "a" * Short.MaxValue
    val listWithMaxCmpWeightElements = List.fill(20)("b" * ContractLimits.MaxCmpWeight.toInt).map(s => s""""$s"""").mkString("[", ",", "]")
    val listWithMaxSizeElements      = List.fill(2)("b" * Short.MaxValue).map(s => s""""$s"""").mkString("[", ",", "]")

    val tooHeavyCmpElement         = maxCmpWeightElement + "a"
    val listWithTooHeavyCmpElement = s""" ("$tooHeavyCmpElement" :: $listWithMaxCmpWeightElements) """

    for (func <- Seq("indexOf", "lastIndexOf")) {
      eval(s""" $listWithMaxSizeElements.$func("$maxCmpWeightElement") """, version = V4) shouldBe Right(unit)
      eval(s""" $listWithMaxCmpWeightElements.$func("$maxSizeElementToFound") """, version = V4) shouldBe Right(unit)

      eval(s""" $listWithMaxSizeElements.$func("$tooHeavyCmpElement") """, version = V4) should produce("are too heavy to compare")
      eval(s""" $listWithTooHeavyCmpElement.$func("$maxSizeElementToFound") """, version = V4) should produce("are too heavy to compare")
    }
  }

  property("list contains") {
    eval(""" ["a","b","c","d"].containsElement("a") """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" ["a","b","c","d"].containsElement("d") """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" [-1,2,3,4].containsElement(-1) """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" [-1,2,3,4].containsElement(4) """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" [true, false].containsElement(true) """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" [true, false].containsElement(false) """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval("""  [base58'a', base58'b', base58'c', base58'd'].containsElement(base58'a') """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval("""  [base58'a', base58'b', base58'c', base58'd'].containsElement(base58'd') """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" ["a","b","c","d"].containsElement("e") """, version = V4) shouldBe Right(CONST_BOOLEAN(false))

    eval(""" [true, false].containsElement(0) """, version = V4) should produce("Can't match inferred types")
    eval(""" [true, false].containsElement() """, version = V4) should produce("Function 'containsElement' requires 2 arguments")
    eval(""" ["a","b","c","d"].containsElement("a") """, version = V3) should produce("Can't find a function 'containsElement'")
  }

  property("list min") {
    eval(""" [1, 2, 3, 4].min() """, version = V4) shouldBe Right(CONST_LONG(1))
    eval(""" [-1, 2, 3, 4].min() """, version = V4) shouldBe Right(CONST_LONG(-1))
    eval(""" [-1, -2, -3, -4].min() """, version = V4) shouldBe Right(CONST_LONG(-4))
    eval(""" [1, 1, 2, 2].min() """, version = V4) shouldBe Right(CONST_LONG(1))
    eval(""" [2].min() """, version = V4) shouldBe Right(CONST_LONG(2))

    eval(""" [].min() """, version = V4) should produce("Can't find min for empty list")
    eval(""" [1, 2].min() """, version = V3) should produce("Can't find a function 'min'")
  }

  property("list max") {
    eval(""" [1, 2, 3, 4].max() """, version = V4) shouldBe Right(CONST_LONG(4))
    eval(""" [-1, 2, 3, 4].max() """, version = V4) shouldBe Right(CONST_LONG(4))
    eval(""" [-1, -2, -3, -4].max() """, version = V4) shouldBe Right(CONST_LONG(-1))
    eval(""" [1, 1, 2, 2].max() """, version = V4) shouldBe Right(CONST_LONG(2))
    eval(""" [2].max() """, version = V4) shouldBe Right(CONST_LONG(2))

    eval(""" [].max() """, version = V4) should produce("Can't find max for empty list")
    eval(""" [1, 2].max() """, version = V3) should produce("Can't find a function 'max'")
  }

  property("ecrecover positive cases") {
    def hash(message: String): String = {
      val prefix = "\u0019Ethereum Signed Message:\n" + message.length
      BaseEncoding.base16().lowerCase().encode(Keccak256.hash((prefix + message).getBytes))
    }

    def recoverPublicKey(message: String, signature: String): Array[Byte] = {
      val script = s"ecrecover(base16'${hash(message)}', base16'$signature')"
      eval[CONST_BYTESTR](script, version = V4).explicitGet().bs.arr
    }

    // source: https://etherscan.io/verifySig/2006
    val signature1 =
      "3b163bbd90556272b57c35d1185b46824f8e16ca229bdb3" +
        "6f8dfd5eaaee9420723ef7bc3a6c0236568217aa990617c" +
        "f292b1bef1e7d1d936fb2faef3d846c5751b"
    val message1         = "what's up jim"
    val expectedAddress1 = "85db9634489b76e238368e4a075cc6e5a56a714c"

    Keys.getAddress(recoverPublicKey(message1, signature1)) shouldBe BaseEncoding.base16().lowerCase().decode(expectedAddress1)

    // source: https://etherscan.io/verifySig/2007
    val signature2 =
      "848ffb6a07e7ce335a2bfe373f1c17573eac320f658ea8" +
        "cf07426544f2203e9d52dbba4584b0b6c0ed5333d84074" +
        "002878082aa938fdf68c43367946b2f615d01b"
    val message2         = "i am the owner"
    val expectedAddress2 = "73f32c743e5928ff800ab8b05a52c73cd485f9c3"

    Keys.getAddress(recoverPublicKey(message2, signature2)) shouldBe BaseEncoding.base16().lowerCase().decode(expectedAddress2)
  }

  property("ecrecover negative cases") {
    eval[CONST_BYTESTR](s"ecrecover(base16'aaaa', base16'bbbb')", version = V3) should
      produce("Can't find a function 'ecrecover'")

    eval[CONST_BYTESTR](s"ecrecover(base16'${"a" * 60}', base16'bbbb')", version = V4) should
      produce("Invalid message hash size 30 bytes, must be equal to 32 bytes")

    eval[CONST_BYTESTR](s"ecrecover(base16'${"a" * 64}', base16'${"a" * 132}')", version = V4) should
      produce("Invalid signature size 66 bytes, must be equal to 65 bytes")

    eval[CONST_BYTESTR](s"ecrecover(base16'${"a" * 64}', base16'${"a" * 130}')", version = V4) should
      produce("Header byte out of range: 197")
  }

  property("n-size generic tuple") {
    lazy val getElement: LazyList[(String, String)] =
      LazyList(
        (""""a"""", "String"),
        ("true", "Boolean"),
        ("123", "Int"),
        ("base58'aaaa'", "ByteVector"),
        ("unit", "Unit")
      ) #::: getElement

    /*  Example for size = 2
     *
     *  let a = (true, 123)
     *  func f(x: (Boolean, Int)) == x
     *
     *  let (a1, a2) = a
     *  a._1 == true && a1 == true && a._2 == 123 && a2 == 123 &&
     *  a == (true, 123) &&
     *  f(a) == a
     */
    def check(size: Int) = {
      val valueDefinition = (1 to size).map(i => getElement(i)._1).mkString("(", ", ", ")")
      val typeDefinition  = (1 to size).map(i => getElement(i)._2).mkString("(", ", ", ")")
      val script =
        s"""
           | let a = $valueDefinition
           | func f(x: $typeDefinition) = x
           |
           | let ${(1 to size).map(i => s"a$i").mkString("(", ", ", ")")} = a
           | ${(1 to size).map(i => s"a._$i == ${getElement(i)._1} && a$i == ${getElement(i)._1} &&").mkString(" ")}
           | a == $valueDefinition &&
           | f(a) == a
         """.stripMargin

      eval(script, version = V3) should produce(s"Can't find a function '$$Tuple$size'")
      eval(script, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    }

    ContractLimits.MinTupleSize to ContractLimits.MaxTupleSize foreach check
  }

  property("tuple match") {
    val script =
      """
        | match(if true then (1, 2) else (true, "q")) {
        |   case _: (Boolean, String) => false
        |   case _: (Int, Int)        => true
        | }
      """.stripMargin

    eval(script, version = V4) shouldBe Right(CONST_BOOLEAN(true))

    val script2 =
      """
        | let a = if (false) then 1 else ("abc", (1, true))
        | let b = if (false) then if (false) then (1, "abc") else (true, "abc") else (base58'', true, a)
        |
        | let c = match b {
        |   case _: (Int | Boolean, String)                          => throw("unxpected 1")
        |   case _: (ByteVector, Boolean, Int)                       => throw("unxpected 2")
        |   case t3: (ByteVector, Boolean, (String, (Int, Boolean))) => t3._3
        | }
        |
        | let d = match b {
        |   case _: (Int, String)                                          => throw("unxpected 3")
        |   case _: (Boolean, String)                                      => throw("unxpected 4")
        |   case t3: (ByteVector, Boolean, Int | (String, (Int, Boolean))) => t3._3
        | }
        |
        | c == ("abc", (1, true)) &&
        | d == ("abc", (1, true))
        |
      """.stripMargin

    eval(script2, version = V4) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("Tuple of unions as function args") {
    val script = """
         func f(x: (Int|String, String|ByteVector)) = {
           x._1
         }
         f((1,"qqq"))"""

    eval(script, version = V4) shouldBe Right(CONST_LONG(1L))
  }

  property("lists of complex types") {
    val script =
      """
        | func f(a: List[Int | (Int, Int)], b: List[List[List[(Int, Int)]]]) = {
        |  match a[0] {
        |   case x: (Int, Int) => x._1 * x._2 + (b[0][0][0]._1)
        |   case y: Int        => y - b[0][0][0]._2
        |  }
        | }
        | f([1], [[[(2,3)]]]) + f([(4,5)], [[[(6,7)]]])
      """.stripMargin

    eval(script, version = V4) shouldBe Right(CONST_LONG(24L))
  }

  val bn256GrothsFail = Seq(
    (
      0,
      s"""
         |let vk = base64'${ByteStr.fill(256)(1).base64}'
         |let proof = base64'CfEpVT6b8+4NAeDs3QwiSN7zqfxzAkQdIu8eBXzoAQIS+AgJcYppUx7COvtbWa7TDtaER1ydtoYWBcBtRMvrHQJ64u4XmLooTwikzECPz+VRcYknrGEoyGeZanNFWEwgplf9bX3JvW1RshlAfN7iJESdqBCmUNsrObHNxhHFJRo='
         |let inputs = base64''
        """.stripMargin
    ),
    (
      1,
      s"""
         |let vk = base64'${ByteStr.fill(256 + 32)(1).base64}'
         |let proof = base64'CfEpVT6b8+4NAeDs3QwiSN7zqfxzAkQdIu8eBXzoAQIS+AgJcYppUx7COvtbWa7TDtaER1ydtoYWBcBtRMvrHQJ64u4XmLooTwikzECPz+VRcYknrGEoyGeZanNFWEwgplf9bX3JvW1RshlAfN7iJESdqBCmUNsrObHNxhHFJRo='
         |let inputs = base64'c9BSUPtO0xjPxWVNkEMfXe7O4UZKpaH/nLIyQJj7iA4='
        """.stripMargin
    ),
    (
      15,
      s"""
         |let vk = base64'${ByteStr.fill(256 + 32 * 15)(1).base64}'
         |let proof = base64'CfEpVT6b8+4NAeDs3QwiSN7zqfxzAkQdIu8eBXzoAQIS+AgJcYppUx7COvtbWa7TDtaER1ydtoYWBcBtRMvrHQJ64u4XmLooTwikzECPz+VRcYknrGEoyGeZanNFWEwgplf9bX3JvW1RshlAfN7iJESdqBCmUNsrObHNxhHFJRo='
         |let inputs = base64'I8C5RcBDPi2n4omt9oOV2rZk9T9xlSV8PQvLeVHjGb00fCVz7AHOIjLJ03ZCTLQwEKkAk9tQWJ6gFTBnG2+0DDHlXcVkwpMafcpS2diKFe0T4fRb0t9mxNzOFiRVcJoeMU1zb/rE4dIMm9rbEPSDnVSOd8tHNnJDkT+/NcNsQ2w0UEVJJRAEnC7G0Y3522RlDLxpTZ6w0U/9V0pLNkFgDCkFBKvpaEfPDJjoEVyCUWDC1ts9LIR43xh3ZZBdcO/HATHoLzxM3Ef11qF+riV7WDPEJfK11u8WGazzCAFhsx0aKkkbnKl7LnypBzwRvrG2JxdLI/oXL0eoIw9woVjqrg6elHudnHDXezDVXjRWMPaU+L3tOW9aqN+OdP4AhtpgT2CoRCjrOIU3MCFqsrCK9bh33PW1gtNeHC78mIetQM5LWZHtw4KNwafTrQ+GCKPelJhiC2x7ygBtat5rtBsJAVF5wjssLPZx/7fqNqifXB7WyMV7J1M8LBQVXj5kLoS9bpmNHlERRSadC0DEUbY9xhIG2xo7R88R0sq04a299MFv8XJNd+IdueYiMiGF5broHD4UUhPxRBlBO3lOfDTPnRSUGS3Sr6GxwCjKO3MObz/6RNxCk9SnQ4NccD17hS/m'
         """.stripMargin
    ),
    (
      16,
      s"""
         |let vk = base64'${ByteStr.fill(256 + 32 * 16)(1).base64}'
         |let proof = base64'CfEpVT6b8+4NAeDs3QwiSN7zqfxzAkQdIu8eBXzoAQIS+AgJcYppUx7COvtbWa7TDtaER1ydtoYWBcBtRMvrHQJ64u4XmLooTwikzECPz+VRcYknrGEoyGeZanNFWEwgplf9bX3JvW1RshlAfN7iJESdqBCmUNsrObHNxhHFJRo='
         |let inputs = base64'I8C5RcBDPi2n4omt9oOV2rZk9T9xlSV8PQvLeVHjGb00fCVz7AHOIjLJ03ZCTLQwEKkAk9tQWJ6gFTBnG2+0DDHlXcVkwpMafcpS2diKFe0T4fRb0t9mxNzOFiRVcJoeMU1zb/rE4dIMm9rbEPSDnVSOd8tHNnJDkT+/NcNsQ2w0UEVJJRAEnC7G0Y3522RlDLxpTZ6w0U/9V0pLNkFgDCkFBKvpaEfPDJjoEVyCUWDC1ts9LIR43xh3ZZBdcO/HATHoLzxM3Ef11qF+riV7WDPEJfK11u8WGazzCAFhsx0aKkkbnKl7LnypBzwRvrG2JxdLI/oXL0eoIw9woVjqrg6elHudnHDXezDVXjRWMPaU+L3tOW9aqN+OdP4AhtpgT2CoRCjrOIU3MCFqsrCK9bh33PW1gtNeHC78mIetQM5LWZHtw4KNwafTrQ+GCKPelJhiC2x7ygBtat5rtBsJAVF5wjssLPZx/7fqNqifXB7WyMV7J1M8LBQVXj5kLoS9bpmNHlERRSadC0DEUbY9xhIG2xo7R88R0sq04a299MFv8XJNd+IdueYiMiGF5broHD4UUhPxRBlBO3lOfDTPnRSUGS3Sr6GxwCjKO3MObz/6RNxCk9SnQ4NccD17hS/mEFt8d4ERZOfmuvD3A0RCPCnx3Fr6rHdm6j+cfn/NM6o='
         """.stripMargin
    )
  )

  property("bn256Groth16Verify_*inputs (false)") {
    for ((ii, lets) <- bn256GrothsFail) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"bn256Groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "bn256Groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  val bn256GrothsOk = Seq(
    (
      0,
      """
        |let vk = base64'oNme33MLprvAodIU3H8rA1WVUVN6IwJtouPFt3rD76EhTyetvNmF9cCLETzYB4K9YC4EIAnZywPddo8kG70hDAeEOBf1FwaXr53SXD0A2pGbHJPRuTTj21tXNXAu7D6MkGKUMCACqi7buNhBbz0X7SKNBgh2Lwxo9CFcv4VBCzkljSjsy0dXI1nUz8oAN99WPRgEqsGtH6wFSllr/AMgKxUVjbIGGpcKvAgvuOfP6HYRsVR8XP7Ecnh/A/aARw4jCycuzPPhJlgjUxs+xCwF/AizkOvYoKFAdFlLKACJIFsIOU8NmXM08eizRXg96hvpfbCVjlWYE1hI90EnJLKTBg=='
        |let proof = base64'G3CEMZl39+IlulSqUjmLUP4tB8pnGGteKKU8AlMzkMMcThMVl9rOa5G3DDcm4iF3BFxS3ubW5JADnVvPhX9wNQXMMjh+BKVLUIaC8gwtwciCscaCMw6cIA5ltWuoWX9jB/ig6Yqg4Hc6u2/9XzKfCWG42Si+BkTh8X0DAQ2fAzY='
        |let inputs = base64''
        |""".stripMargin
    ),
    (
      1,
      """
        |let vk = base64'LDCJzjgi5HtcHEXHfU8TZz+ZUHD2ZwsQ7JIEvzdMPYKYs9SoGkKUmg1yya4TE0Ms7x+KOJ4Ze/CPfKp2s5jbniFNM71N/YlHVbNkytLtQi1DzReSh9SNBsvskdY5mavQJe+67PuPVEYnx+lJ97qIG8243njZbGWPqUJ2Vqj49NAunhqX+eIkK3zAB3IPWls3gruzX2t9wrmyE9cVVvf1kgWx63PsQV37qdH0KcFRpCH89k4TPS6fLmqdFxX3YGHCGFTpr6tLogvjbUFJPT98kJ/xck0C0B/s8PTVKdao4VQHT4DBIO8+GB3CQVh6VV4EcMLtDWWNxF4yloAlKcFT0Q4AzJSimpFqd/SwSz9Pb7uk5srte3nwphVamC+fHlJt'
        |let proof = base64'GQPBoHuCPcIosF+WZKE5jZV13Ib4EdjLnABncpSHcMKBZl0LhllnPxcuzExIQwhxcfXvFFAjlnDGpKauQ9OQsjBKUBsdBZnGiV2Sg4TSdyHuLo2AbRRqJN0IV3iH3On8I4ngnL30ZAxVyGQH2EK58aUZGxMbbXGR9pQdh99QaiE='
        |let inputs = base64'IfZhAypdtgvecKDWzVyRuvXatmFf2ZYcMWVkCJ0/MQo='
        |""".stripMargin
    ),
    (
      2,
      """
        |let vk = base64'nV33RK4rTODU42ZKyFxTFl9d86FrP+h6acIdT4m/rfAZBBfPWLxcjYyMUBHlXgM/jTBDOH7dTGL1zqbRLr1eGwR9zemG9LJnqPl/eiJ0LZLZKz3/iDde8p1zj5DRvar2Fa6rv0WJRJR32+22iaZHrD/64/SyFb2j5f12ipT5S2Eq+SnYSSr8HhStVu3s4VFK57nhi2aRFUgXWkGaqhJJ6ie6zlaVf52s79qdlBUOuTTVsATXa58FVXVSHJb5wanwK578EWV/BulP1TCq5y0q7k6YCZV15Nu9FHpzIUow9Ged8l8LwPmHxki+/S2MnA0v2mFgdC1ZXE+BesWOx3tXThrq7st5+lUMmaY5S76l0yuzzHoeSZEZAoS6heG8WWUGJW2QBTzq3GVaubssoR/HtIT1dGPswNTF8HY78BPYb8M='
        |let proof = base64'pE0Y4zE1A5UvC8ATBJPMOAFm3dDQABrhu5VxxGj5GOom+pFpQsVZe6mMLT8ZwyWXQmJ+od+cYtUOH2Fxiem1yCdMkGi64f5k9qWwYDRrtIxXHhj9f43pHKylJV0Z1jCJgpebExv+hPmwNYh3cWxHxywYMEXwSutAjyqi8Swl1O0='
        |let inputs = base64'IfZhAypdtgvecKDWzVyRuvXatmFf2ZYcMWVkCJ0/MQou2EkLZ569itz7cL7GQnzipNe1JRCQ/QK8UXr8IG8k0g=='
        |""".stripMargin
    ),
    (
      4,
      """
        |let vk = base64'q1U6rGRU+/F+e3xx6oCBeokQsIVIQDESLC/l+PK8WX0Rw2NglYMRP6In971j7/puuDsJwZm2AK7fTcmKQd5v/g6InaZLdKGQK1C+52BbMyoGsueMfmML9pJmJvivE2N7m9WyiixGvDawwhz7JYst5Jr3ChczYgLAS7b2WQscCRcm8ZE9FKTNdfu4WAYEDEl6IONCFnNiYw6wR8Kc2J7Z9BkHcOzQj2X4PuxZGdPeWF+7vB1dXQZZlCBScImWeVPNKaYF7H3Ot0wOYzigQXYkSZD5v9vBWXN/f1YajxR1kjMrSZ90fx4WapypjaLREbaNgOcAN5niSzTmwymK3e3Idgpt+Pdq34YPOZ5lDx5+3gMWYl1QYeWi6miDIasts0owjZVOTUXf292iZDsjSWaYurcLNEH3Rf8lTCKSMyosjkAV2Fkselh0/Vy4jkPj3hTFeIpY3inMmI+N6Nvwxv50ZpaUIQmU5rlia4pS8MSMxgO+khyyPKb1OUHbFTgDHf6p'
        |let proof = base64'mtMjjG0IVNq3t7ICtij1nLFH1UjFu6FaNZmdXam0gdCY/efE2wVHSctp1vqgVcfgiSpl/WCCcaQ2CQGbirLE/wVj2w0eiVEUy6gs9aK01nS0bx4ErymppOugPDPXGCV6FtiAM6Cq+fMbOKhhhDPutn6dIntO3gSqtsWL0KreCPY='
        |let inputs = base64'IfZhAypdtgvecKDWzVyRuvXatmFf2ZYcMWVkCJ0/MQou2EkLZ569itz7cL7GQnzipNe1JRCQ/QK8UXr8IG8k0hW3GAoAXun/Pwk0Jq9fst26FET2RehSLbxUdvZQLjEzCJmfahCreklII85wEMuwYctsOv/3JWgvrI8hmn8sWjw='
        |""".stripMargin
    ),
    (
      8,
      """
        |let vk = base64'kikeX/IlYwL5e1nkJ3FbD4/IantLORJJtmgMPywFCoCdYF304ka26knjzRodk7mujA0HqXxknBkzZSqQuuqcSSZkVOudYO1dwOw73U9SlL/nF4UWP4YOkVfKntqi825LkCrBcl3rw7zhMHorldspOwWNVybCLGt86Zbd92hze3wcSIk2GqMxtoBQiurYqlt1SARwIn06tRExJ0YULl/7/4qrYi+Tsjy0xQwltrHJQy/eeVNNq9x/GrVJemN2r+SUK6lF6kWLJzTH5jUUu7HvpUCtRlGJ3JRZIAJ7qChT3v+fD5LNjz86Ei1ItV4Wgqk/iXAAXeORUM3T3RD2NepVgS3U35HXhJ1ZtzJZoFUQyQjlGYp6+U6q8+rXYSQkZjEHmQh/Atr0lb1QRIG+PK+mLj1Nxwb/aibt7z7WtRTfj3mudYCEjXA/ceDVtz8FKkeTo9dcVYGsf/VJaxnteVa7B4UMRQjX0gdNKC+AEjKxcRk373IqLBCCtukQYBG8o2TKFYfJcJuI1geuGtNij1jwgmLxl7hH3Y0JqMZPoS9vKwudBL5Nnt38g/rfNTyoe7UIJV246oAOkuuskfxNUHeEnILjkHYhjfIuuPMGUG4FK2LHkscqib6eHo3wSy31I65urmsiHROrMEtfUdYcGDJxV+IyrTUs2m5KQgmlXo8jvUo='
        |let proof = base64'HUaOoIyn6KUkVXzttFMcRobGBZUtpRjAQMId9n3ABZKPUk8Rv0EPKmzp/1Ut0LNJ5cNI48VwD6/kVbhNgoBbdQijsqDktFjjeJtzj6KF2TuHOlcHL6s7dc38cvNVD0O1ISiJlLdrc5QKcXePAGJK/YaD/CUQfKnijgCGDREUDXo='
        |let inputs = base64'IfZhAypdtgvecKDWzVyRuvXatmFf2ZYcMWVkCJ0/MQou2EkLZ569itz7cL7GQnzipNe1JRCQ/QK8UXr8IG8k0hW3GAoAXun/Pwk0Jq9fst26FET2RehSLbxUdvZQLjEzCJmfahCreklII85wEMuwYctsOv/3JWgvrI8hmn8sWjwELKgjqMSLQgJ7u5ZHe9PDhBgF/3dliXhY1jUsXFvkig6pfEr0bFuXhXcg2R+vuPErhG0w08SPNOi2SjvPngYpJ8nsOT98G6EDlyNaHtJuRC/xOA+6ftL/k2+hzFsd+1Ui5/1NGTfDxLDndEE1NQ+opvk79ZeaY+qPP7pEc0uQMw=='
        |""".stripMargin
    ),
    (
      16,
      """
        | let vk = base64'qladoBQdTIMTqRD5nLplvCqxlHwZrFv7scw72CqqjkMsK1J9mo3aYYCS8MXIl224CTjyJF/IQ2nFONXeSUdoFAFN1RfVylmeW3Qbeqj8ePv9JA+LjpgE68Zr9u85DqI4hK+0BABhe1S+m79CtKaSOAt7pJNDmiNqEKsGYDBhYBIBLAdgsmLkOfA197v06p/UaXCOXvNQMjprjuvt3SCrkoV8jZU/cSrB+cPPE8s4OgVl0eXr08YRmmDym/veV4eTI1WV59Qxb3uZCOtJFPwwd6gTiXvnGhhWrgJPwjAIJhmFDEUI19IHTSgvgBIysXEZN+9yKiwQgrbpEGARvKNkyhWHyXCbiNYHrhrTYo9Y8IJi8Ze4R92NCajGT6EvbysLnQS+TZ7d/IP63zU8qHu1CCVduOqADpLrrJH8TVB3hJyC45B2IY3yLrjzBlBuBStix5LHKom+nh6N8Est9SOubq5rIh0TqzBLX1HWHBgycVfiMq01LNpuSkIJpV6PI71KmXEqw09G6gPAM1465XNSWZV0gLUTctlavUWfG7jILPafEBGrM69wVNmnBm0lSOq9fBTatb8Ivm4+SDNHALraRo1guaelsx+MuKox7hj5WkuIjo7PSprYHsM6Wc3/10VpJQwX6/Dp5J0MZzYscKLlGX5DXXbaQLcIT6I+cjioLcIwK20hLc56+CaCSZRyKMB9IWUmavploHQrjBW+vyyOPQNIbqUTWjTVHJ9QCCdVxUWP+0yHwCjZUymqnVoG6HrPkYvh0nuIsz552K6SWFMuhddTW+JN/uUIpniAKCI4WwafIl/0mH/DRktCA5uQdpevX62mWKfyYGL1if6TV20CgSMvo6fiK+yC5GCzMKtsHxMFWW5fYkjQ2b/C8RWjCySpDQPFVqJwr5uMYxdqtsSs7ysGZfpoRZS1SDbVgFVL1E6d/ECJMiqIOM0OH0uBRzF7B3q2BT5GChq/naHPEucCcBU8HgairQQ3uV+V+UyWbYmrwjGQZSg0pSQ3Jff41MGe'
        | let proof = base64'CfEpVT6b8+4NAeDs3QwiSN7zqfxzAkQdIu8eBXzoAQIS+AgJcYppUx7COvtbWa7TDtaER1ydtoYWBcBtRMvrHQJ64u4XmLooTwikzECPz+VRcYknrGEoyGeZanNFWEwgplf9bX3JvW1RshlAfN7iJESdqBCmUNsrObHNxhHFJRo='
        | let inputs = base64'IfZhAypdtgvecKDWzVyRuvXatmFf2ZYcMWVkCJ0/MQou2EkLZ569itz7cL7GQnzipNe1JRCQ/QK8UXr8IG8k0hW3GAoAXun/Pwk0Jq9fst26FET2RehSLbxUdvZQLjEzCJmfahCreklII85wEMuwYctsOv/3JWgvrI8hmn8sWjwELKgjqMSLQgJ7u5ZHe9PDhBgF/3dliXhY1jUsXFvkig6pfEr0bFuXhXcg2R+vuPErhG0w08SPNOi2SjvPngYpJ8nsOT98G6EDlyNaHtJuRC/xOA+6ftL/k2+hzFsd+1Ui5/1NGTfDxLDndEE1NQ+opvk79ZeaY+qPP7pEc0uQMxnEIHHpcYsRt9dal9bSQpE5hWKu1nOBzIVmXb/Ef51YDg+nW5w9a2tEAY2zQCZ/z3sFs7FwAZ5TXhDfhYR5sQ8tZaF3FWh+Yzf5hgMmXWrApp/arwPszNhKCoxScnhPSgfcxYSdauqvp5+vcacJFY1OkWG6tQ6iuh5CZSdX645oA7oNr9d5kXYSewhTflcV9pufeaH21BtEipHpO3sNRkUngnHC+uj1D8ReSgcCofnv8s0mVme9Ml64r6CbeaHK+x5Mc9bolN96XJZ137xkPDpev+RVrVK6ZIFrH2hFl8/vGoBwWDlmjmVzUt4YQdsCavsf7c0vBa7d33EcvyvQMDY='
         """.stripMargin
    )
  )

  property("bn256Groth16Verify_*inputs") {
    for ((ii, lets) <- bn256GrothsOk if (ii <= 16)) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"bn256Groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "bn256Groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    }
  }

  property("bn256Groth16Verify_*inputs fail if too many inputs") {
    for ((i, lets) <- bn256GrothsFail ++ bn256GrothsOk if (i > 1)) {
      val src = lets ++ (if (i <= 16) {
                           s"bn256Groth16Verify_${i - 1}inputs(vk, proof, inputs)"
                         } else {
                           "bn256Groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Left(s"Invalid inputs size ${i * 32} bytes, must be not greater than ${i * 32 - 32} bytes")
    }
  }

  property("bn256Groth16Verify_*inputs with invalid vk size") {
    for {
      ii <- 0 to 16
      n  <- Seq(0, (8 + ii) * 48 + 1, (8 + ii) * 48 - 1)
    } {
      val lets = s"""
                    |let vk = base16'${"AA" * n}'
                    |let proof = base64'CfEpVT6b8+4NAeDs3QwiSN7zqfxzAkQdIu8eBXzoAQIS+AgJcYppUx7COvtbWa7TDtaER1ydtoYWBcBtRMvrHQJ64u4XmLooTwikzECPz+VRcYknrGEoyGeZanNFWEwgplf9bX3JvW1RshlAfN7iJESdqBCmUNsrObHNxhHFJRo='
                    |let inputs = base16'${"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" * ii}'
                    |""".stripMargin
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"bn256Groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "bn256Groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Left(s"Invalid vk size ${n} bytes, must be equal to ${(8 + ii) * 32} bytes for ${ii} inputs")
    }
  }

  property("bn256Groth16Verify_*inputs with invalid proof size") {
    def lets(proofSize: Int) =
      s"""
         |let vk = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
         |let proof = base64'${ByteStr.fill(proofSize)(1).base64}'
         |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs='
         |""".stripMargin
    for (ii <- 0 to 16; proofSize <- List(127, 129)) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets(proofSize) ++ (if (i != 16) {
                                      s"bn256Groth16Verify_${i}inputs(vk, proof, inputs)"
                                    } else {
                                      "bn256Groth16Verify(vk, proof, inputs)"
                                    })
      eval(src, version = V4) shouldBe Left(s"Invalid proof size $proofSize bytes, must be equal to 128 bytes")
    }
  }

  property("bn256Groth16Verify_*inputs with invalid inputs size") {
    val lets =
      """
        |let vk = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
        |let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
        |let inputs = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs=' + base16'00'
        |""".stripMargin
    for (ii <- 2 to 16) {
      val i = if (ii == 0) {
        1
      } else {
        ii
      }
      val src = lets ++ (if (i != 16) {
                           s"bn256Groth16Verify_${i}inputs(vk, proof, inputs)"
                         } else {
                           "bn256Groth16Verify(vk, proof, inputs)"
                         })
      eval(src, version = V4) shouldBe Left("Invalid inputs size 33 bytes, must be a multiple of 32 bytes")
    }
  }

  property("removeByIndex") {
    eval(" [1, 2, 3, 4].removeByIndex(0) == [2, 3, 4] ", version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(" [1, 2, 3, 4].removeByIndex(1) == [1, 3, 4] ", version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(" [1, 2, 3, 4].removeByIndex(2) == [1, 2, 4] ", version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(" [1, 2, 3, 4].removeByIndex(3) == [1, 2, 3] ", version = V4) shouldBe Right(CONST_BOOLEAN(true))

    eval(" [1, 2, 3, 4].removeByIndex(-1)", version = V4) should produce(
      "Index of the removing element should be positive, but -1 was passed"
    )

    eval(" [1, 2, 3, 4].removeByIndex(4)", version = V4) should produce(
      "Index of the removing element should be lower than list size = 4, but 4 was passed"
    )

    eval(" [1, 2, 3, 4].removeByIndex(5)", version = V4) should produce(
      "Index of the removing element should be lower than list size = 4, but 5 was passed"
    )

    eval(" [].removeByIndex(0)", version = V4) should produce(
      "Can't remove an element from empty list"
    )

    eval(" [1, 2, 3, 4].removeByIndex(0)", version = V3) should produce(
      "Can't find a function 'removeByIndex'(List[Int], Int)"
    )
  }

  property("Union with single List") {
    eval("""match (if false then 2 else [3]) { case n: Int => n case a => a[0] }""", version = V4) shouldBe Right(CONST_LONG(3))
  }

  property("Union with multiple List") {
    eval("""match (if false then 2 else if true then [3] else ["qqq"]) { case n: Int => n case a => a[0] }""", version = V4) shouldBe Right(
      CONST_LONG(3)
    )
  }

  property("Any type") {
    eval(
      """func f(x:Any) = {
           match x { case n: Int => n*4-1 case a => 4 }
    }
    f(1)""",
      version = V4
    ) shouldBe Right(CONST_LONG(3))
    eval(
      """func f(x:Any) = {
           match x { case n: Int => n*4-1 case a => 4 }
    }
    f("q")""",
      version = V4
    ) shouldBe Right(CONST_LONG(4))
    eval(
      """func f(x: Any) = { x+1 }
            f(1)""",
      version = V4
    ) shouldBe Symbol("left")

  }

  property("extracting data functions with DeleteEntry") {
    val script =
      """
        | let deleteEntry  = DeleteEntry("delete")
        | let integerEntry = IntegerEntry("integer", 123)
        | let stringEntry  = StringEntry("string", "value")
        | let booleanEntry = BooleanEntry("boolean", true)
        | let binaryEntry  = BinaryEntry("binary", base58'a')
        |
        | let entries = [deleteEntry, integerEntry, stringEntry, booleanEntry, binaryEntry]
        |
        | getIntegerValue(entries, "integer") == 123       &&
        | getIntegerValue(entries, 1)         == 123       &&
        | getStringValue(entries, "string")   == "value"   &&
        | getStringValue(entries, 2)          == "value"   &&
        | getBooleanValue(entries, "boolean") == true      &&
        | getBooleanValue(entries, 3)         == true      &&
        | getBinaryValue(entries, "binary")   == base58'a' &&
        | getBinaryValue(entries, 4)          == base58'a' &&
        | entries[0] == deleteEntry
      """.stripMargin

    val ctx = WavesContext.build(Global, DirectiveSet(V4, Account, DApp).explicitGet(), fixBigScriptField = true)
    genericEval(script, ctxt = ctx, version = V4, env = utils.environment) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("List[Any]") {
    eval(
      """func f(x: List[Int]|Int) = {
      match x { case a: List[Any] => 4 case n: Int => n*4-1 }
    }
    f([1])""",
      version = V4
    ) shouldBe Right(CONST_LONG(4))
  }

  property("value(Any)") {
    eval(
      """func f(x: Any) = {
      value(x)
    }
    f(1)""",
      version = V4
    ) shouldBe Symbol("Left")
  }

  property("default type") {
    eval(
      """func f(x: Int| String) = {
      match x {
        case i: Int => 1
        case v =>
          match v {
            case j: Int => 2
            case s: String => 3
          }
      }
    }
    f("q")""",
      version = V4
    ) shouldBe Symbol("Left")
  }

  property("different Lease action constructors") {
    val script = " Lease(Address(base58''), 1234567) == Lease(Address(base58''), 1234567, 0) "
    genericEval[Environment, EVALUATED](script, ctxt = v5Ctx, version = V5, env = utils.environment) shouldBe
      Right(CONST_BOOLEAN(true))
  }

  property("calculateLeaseId") {
    val txId = ByteStr.decodeBase58("aaaa").get
    val id1  = Lease.calculateId(Lease(Address(ByteStr.decodeBase58("bbbb").get), 1234567, 123), txId)
    val id2  = Lease.calculateId(Lease(Alias("alias"), 9876, 100), txId)
    val script =
      s"""
         | calculateLeaseId(Lease(Address(base58'bbbb'), 1234567, 123)) == base58'$id1' &&
         | calculateLeaseId(Lease(Alias("alias"), 9876, 100))           == base58'$id2' &&
         | base58'$id1' != base58'$id2'
       """.stripMargin
    genericEval[Environment, EVALUATED](script, ctxt = v5Ctx, version = V5, env = utils.buildEnvironment(txId)) shouldBe
      Right(CONST_BOOLEAN(true))
  }

  property("calculateLeaseId restrictions") {
    val script1 = s" calculateLeaseId(Lease(Address(base58'${"a" * 36}'), 1234567, 123)) "
    val script2 = s""" calculateLeaseId(Lease(Alias("${"a" * 31}"), 1234567, 123)) """

    genericEval[Environment, EVALUATED](script1, ctxt = v5Ctx, version = V5, env = utils.environment) should
      produce("Address bytes length=27 exceeds limit=26")
    genericEval[Environment, EVALUATED](script2, ctxt = v5Ctx, version = V5, env = utils.environment) should
      produce("Alias name length=31 exceeds limit=30")
  }

  property("integer case") {
    val sampleScript =
      """match 2 {
        |  case 1 => 7
        |  case 3 | 2 => 8
        |  case _ => 9
        |}""".stripMargin
    eval[EVALUATED](sampleScript, None) shouldBe evaluated(8)
  }

  property("string case") {
    val sampleScript =
      """match "qq" {
        |  case "1" => 7
        |  case "qq" | "2" => 8
        |  case _ => 9
        |}""".stripMargin
    eval[EVALUATED](sampleScript, None) shouldBe evaluated(8)
  }

  property("binary case") {
    val sampleScript =
      """match base64'TElLRQ==' {
        |  case base64'TElLRQ==' => 7
        |  case base64'ZGdnZHMK' | base64'ZGdnZHMJ' => 8
        |  case _ => 9
        |}""".stripMargin
    eval[EVALUATED](sampleScript, None) shouldBe evaluated(7)
  }
  property("tuple destruct") {
    val sampleScript =
      """|
         |match (5, "qqq") {
         |  case (n, "qqq") => n
         |  case _  => (1, "ggg")
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, version = V6) shouldBe evaluated(5)
  }

  property("typed tuple destruct") {
    val sampleScript =
      """|
         |match (5, if true then "qqq" else base64'') {
         |  case (5, n : ByteVector) => "ttt"
         |  case (5|4, n : String) => n
         |  case _  => "ggg"
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, version = V4) shouldBe evaluated("qqq")
  }

  property("caseType destruct") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case PointC(YB=n) => n
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(42)
  }

  property("caseType destruct with type checking") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case PointC(YB=n:Int) => n
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(42)
  }

  property("caseType constant field") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case PointC(YB=24) => 2
         |  case PointC(YB=42) => 6
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(6)
  }
}
