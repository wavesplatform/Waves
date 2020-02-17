package com.wavesplatform.lang

import cats.Id
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Testing._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{BYTESTR, FINAL, LONG, STRING}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, _}
import com.wavesplatform.lang.v1.evaluator.{Contextful, ContextfulVal, EvaluatorV1}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Issue
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Random

class IntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {
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
    val untyped = Parser.parseExpr(code).get.value

    val f: BaseFunction[C] =
      NativeFunction(
        "fn1",
        1,
        91:Short,
        pointType,
        ("value", pointType),
      ) {
        case _ :: Nil => throw new Exception("test exception")
        case xs => notImplemented[Id]("fraction(value: Int, numerator: Int, denominator: Int)", xs)
      }

    val lazyVal       = ContextfulVal.pure[C](pointInstance.orNull)
    val stringToTuple = Map(("p", (pointType, lazyVal)))
    val ctx: CTX[C] =
      Monoid.combineAll(
        Seq(
          PureContext.build(Global, version).withEnvironment[C],
          CryptoContext.build(Global, version).withEnvironment[C],
          addCtx.withEnvironment[C],
          CTX[C](sampleTypes, stringToTuple, Array(f)),
          ctxt
        )
      )

    val typed = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => new EvaluatorV1[Id, C]().apply(ctx.evaluationContext(env), v._1))
  }

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
        |  case pa: PointA => let x = 3
        |  case _ => throw()
        |}
      """.stripMargin
    eval[EVALUATED](src) should produce("can't parse the expression")
  }

  property("Exception handling") {
    val sampleScript =
      """match fn1(p) {
        |  case pa: PointA => 0
        |  case pa: PointB => 1
        |  case pa: PointC => 2
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe 'Left
  }

  property("patternMatching") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointB => 1
        |  case pa: PointC => 2
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("patternMatching with named union types") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointBC => 1
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
    ExpressionCompiler.compile(script, CTX.empty.compilerContext)

  property("wrong script return type") {
    compile("1") should produce("should return boolean")
    compile(""" "string" """) should produce("should return boolean")
    compile(""" base58'string' """) should produce("should return boolean")
  }

  property("equals works on elements from Gens") {
    List(CONST_LONGgen, SUMgen(50), INTGen(50)).foreach(
      gen =>
        forAll(for {
          (expr, res) <- gen
          str         <- toString(expr)
        } yield (str, res)) {
          case (str, res) =>
            withClue(str) {
              eval[EVALUATED](str) shouldBe evaluated(res)
            }
        }
    )

    forAll(for {
      (expr, res) <- BOOLgen(50)
      str         <- toString(expr)
    } yield (str, res)) {
      case (str, res) =>
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
        | case y: String => 2
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
        |  case other => throw()
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(3)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(42)
  }

  property("different types, same name of field") {
    val sampleScript =
      """match (p.YB) {
        | case l: Int => l
        | case u: Unit => 1
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
        |  case a: PointA => 0
        |  case b: PointB => throw()
        |  case c: PointC => throw("arrgh")
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
      PureContext.build(Global, V1).evaluationContext[Id],
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
      PureContext.build(Global, V1).evaluationContext[Id],
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
        )
      )
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
    val arrVal = ContextfulVal.pure[NoContext](CONST_BYTESTR(array).explicitGet())
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

  property("indexOf") {
    val src =
      """ "qweqwe".indexOf("we") """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(1L))
  }

  property("indexOf with zero offset") {
    val src =
      """ "qweqwe".indexOf("qw", 0) """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(0L))
  }

  property("indexOf with start offset") {
    val src =
      """ "qweqwe".indexOf("we", 2) """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(4L))
  }

  property("indexOf from end of max sized string") {
    val str = "a" * 32766 + "z"
    val src =
      """ str.indexOf("z", 32766) """
    eval[EVALUATED](
      src,
      ctxt = CTX[NoContext](
        Seq(),
        Map("str" -> (STRING -> ContextfulVal.pure[NoContext](CONST_STRING(str).explicitGet()))),
        Array()
      )
    ) shouldBe Right(CONST_LONG(32766L))
  }

  property("indexOf (not present)") {
    val src =
      """ "qweqwe".indexOf("ww") """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("indexOf from empty string") {
    val src =
      """ "".indexOf("!") """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("indexOf from empty string with offset") {
    val src =
      """ "".indexOf("!", 1) """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("indexOf from string with Long.MaxValue offset") {
    val src =
      s""" "abc".indexOf("c", ${Long.MaxValue}) """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("indexOf from string with negative offset") {
    val src =
      """ "abc".indexOf("a", -1) """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("indexOf from string with negative Long.MinValue offset") {
    val src =
      s""" "abc".indexOf("a", ${Long.MinValue}) """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("indexOf empty string from non-empty string") {
    val src =
      """ "abc".indexOf("") """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(0))
  }

  property("indexOf empty string from empty string") {
    val src =
      """ "".indexOf("") """
    eval[EVALUATED](src) shouldBe Right(CONST_LONG(0))
  }

  property("lastIndexOf") {
    val src =
      """ "qweqwe".lastIndexOf("we") """
    eval(src) shouldBe Right(CONST_LONG(4))
  }

  property("lastIndexOf with zero offset") {
    val src =
      """ "qweqwe".lastIndexOf("qw", 0) """
    eval(src) shouldBe Right(CONST_LONG(0))
  }

  property("lastIndexOf with start offset") {
    val src =
      """ "qweqwe".lastIndexOf("we", 4) """
    eval(src) shouldBe Right(CONST_LONG(4L))
  }

  property("lastIndexOf from end of max sized string") {
    val str = "a" * 32766 + "z"
    val src =
      """ str.lastIndexOf("z", 32766) """
    eval(src, ctxt = CTX[NoContext](Seq(), Map("str" -> (STRING -> ContextfulVal.pure[NoContext](CONST_STRING(str).explicitGet()))), Array())) shouldBe Right(
      CONST_LONG(32766L)
    )
  }

  property("lastIndexOf (not present)") {
    val src =
      """ "qweqwe".lastIndexOf("ww") """
    eval(src) shouldBe Right(unit)
  }

  property("lastIndexOf from empty string") {
    val src =
      """ "".lastIndexOf("!") """
    eval(src) shouldBe Right(unit)
  }

  property("lastIndexOf from empty string with offset") {
    val src =
      """ "".lastIndexOf("!", 1) """
    eval(src) shouldBe Right(unit)
  }

  property("lastIndexOf from string with Int.MaxValue offset") {
    val src =
      s""" "abc".lastIndexOf("c", ${Int.MaxValue}) """
    eval(src) shouldBe Right(CONST_LONG(2))
  }

  property("lastIndexOf from string with Long.MaxValue offset") {
    val src =
      s""" "abc".lastIndexOf("c", ${Long.MaxValue}) """
    eval(src) shouldBe Right(CONST_LONG(2))
  }

  property("lastIndexOf from string with negative offset") {
    val src =
      """ "abc".lastIndexOf("a", -1) """
    eval(src) shouldBe Right(unit)
  }

  property("lastIndexOf from string with negative Long.MinValue offset") {
    val src =
      s""" "abc".lastIndexOf("a", ${Long.MinValue}) """
    eval(src) shouldBe Right(unit)
  }

  property("lastIndexOf empty string from non-empty string") {
    val str = "abcde"
    val src =
      s""" "$str".lastIndexOf("") """
    eval(src) shouldBe Right(CONST_LONG(str.length))
  }

  property("lastIndexOf empty string from empty string") {
    val src =
      """ "".lastIndexOf("") """
    eval(src) shouldBe Right(CONST_LONG(0))
  }

  property("split") {
    val src =
      """ "q:we:.;q;we:x;q.we".split(":.;") """
    eval[EVALUATED](src) shouldBe Right(
      ARR(
        IndexedSeq(
          CONST_STRING("q:we").explicitGet(),
          CONST_STRING("q;we:x;q.we").explicitGet()
        )
      )
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
        )
      )
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
        )
      )
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
        )
      )
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
    eval[EVALUATED](src) shouldBe 'left
  }

  property("parseInt fail when string ends with non-digit") {
    val src =
      """ "42x".parseInt() """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("parseIntValue fail when string ends with non-digit") {
    val src =
      """ "42x".parseIntValue() """
    eval[EVALUATED](src) shouldBe 'left
  }

  property("parseInt fail when string is empty") {
    val src =
      """ "".parseInt() """
    eval[EVALUATED](src) shouldBe Right(unit)
  }

  property("parseIntValue fail when string is empty") {
    val src =
      """ "".parseIntValue() """
    eval[EVALUATED](src) shouldBe 'left
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
    eval[EVALUATED]("Down() == DOWN", None) should produce("Can't find a function")
    eval[EVALUATED]("Up() == UP", None) should produce("Can't find a function")
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

  property("math functions") {
    eval[EVALUATED]("pow(12, 1, 3456, 3, 2, DOWN)", None) shouldBe Right(CONST_LONG(187))
    eval[EVALUATED]("pow(12, 1, 3456, 3, 2, UP)", None) shouldBe Right(CONST_LONG(188))
    eval[EVALUATED]("pow(0, 1, 3456, 3, 2, UP)", None) shouldBe Right(CONST_LONG(0))
    eval[EVALUATED]("pow(20, 1, -1, 0, 4, DOWN)", None) shouldBe Right(CONST_LONG(5000))
    eval[EVALUATED]("pow(-20, 1, -1, 0, 4, DOWN)", None) shouldBe Right(CONST_LONG(-5000))
    eval[EVALUATED]("pow(0, 1, -1, 0, 4, DOWN)", None) shouldBe 'left
    eval[EVALUATED]("log(16, 0, 2, 0, 0, CEILING)", None) shouldBe Right(CONST_LONG(4))
    eval[EVALUATED]("log(16, 0, -2, 0, 0, CEILING)", None) shouldBe 'left
    eval[EVALUATED]("log(-16, 0, 2, 0, 0, CEILING)", None) shouldBe 'left
  }

  property("math functions scale limits") {
    eval("pow(2,  0, 2, 9, 0, UP)") should produce("out of range 0-8")
    eval("log(2,  0, 2, 9, 0, UP)") should produce("out of range 0-8")
    eval("pow(2, -2, 2, 0, 5, UP)") should produce("out of range 0-8")
    eval("log(2, -2, 2, 0, 5, UP)") should produce("out of range 0-8")
  }

  property("pow result size max") {
    eval("pow(2, 0, 62, 0, 0, UP)") shouldBe Right(CONST_LONG(Math.pow(2, 62).toLong))
    eval("pow(2, 0, 63, 0, 0, UP)") should produce("out of long range")
  }

  property("pow result size abs min") {
    eval("pow(10, 0, -8, 0, 8, HALFUP)") shouldBe Right(CONST_LONG(1))
    eval("pow(10, 0, -9, 0, 8, HALFUP)") shouldBe Right(CONST_LONG(0))
  }

  property("HalfUp is type") {
    eval("let r = if true then HALFUP else HALFDOWN ; match r { case _:HalfUp => 1 case _ => 0 }") shouldBe Right(CONST_LONG(1))
  }


  property("HalfUp type have no constructor") {
    eval("pow(10, 0, -8, 0, 8, HalfUp())") shouldBe 'Left
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
         |   case s: String => false
         | } &&
         | match n[0] {
         |   case i: Int => false
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
         |
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

  property("string contains") {
    eval(""" "qwerty".contains("we") """, version = V3) should produce("Can't find a function")
    eval(""" "qwerty".contains("we") """, version = V4) shouldBe Right(CONST_BOOLEAN(true))
    eval(""" "qwerty".contains("xx") """, version = V4) shouldBe Right(CONST_BOOLEAN(false))
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
       """.stripMargin

    val scriptResultScript =
      s"""
         | func f(sr: ScriptResult) = true
         | true
         |
       """.stripMargin

    val ctx = WavesContext.build(DirectiveSet(V4, Account, DApp).explicitGet())

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
    ) should produce("non-existing type")
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

  property("List[Int] median - 101 elements - error") {
    val arr = (1 to 101).map(_ => Long.MaxValue)
    val src =
      s"[${arr.mkString(",")}].median()"
    eval(src, version = V4) should produce("Invalid list size. Size should be between 1 and")
  }

  property("List[Int] median - 500 elements - error") {
    val arr = (1 to 500).map(_ => Long.MaxValue)
    val src =
      s"[${arr.mkString(",")}].median()"
    eval(src, version = V4) should produce("Invalid list size. Size should be between 1 and")
  }

  property("List[Int] median - empty list - error") {
    val src =
      s"[].median()"
    eval(src, version = V4) should produce("Invalid list size. Size should be between 1 and")
  }

  property("List[Int] median - list with non int elements - error") {
    val src =
      s"""["1", "2"].median()"""
    eval(src, version = V4) should produce("Compilation failed: [Non-matching types: expected: Int, actual: String")
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

  property("calculateAssetId") {
    val decimals = 100
    val description = "description"
    val isReissuable = true
    val name = "name"
    val quantity = 1234567
    val nonce = 1
    val issue = Issue.create(compiledScript = None, decimals, description, isReissuable, name, quantity, nonce, ByteStr.empty)
    val script =
     s"""
        | let issue = Issue(unit, $decimals, "$description", $isReissuable, "$name", $quantity, $nonce)
        | calculateAssetId(issue)
      """.stripMargin

    val ctx = WavesContext.build(DirectiveSet(V4, Account, DApp).explicitGet())

    genericEval[Environment, EVALUATED](script, ctxt = ctx, version = V4, env = utils.environment) shouldBe
      CONST_BYTESTR(issue.id)
  }
}
