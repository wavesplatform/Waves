package com.wavesplatform.lang.evaluator.string

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, EXPR, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{GlobalValNames, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import com.wavesplatform.test.produce

class MakeStringTest extends EvaluatorSpec {
  implicit val v: StdLibVersion = V6

  property("correct results") {
    for (f <- List(PureContext.makeString, PureContext.makeString_V6, PureContext.makeString_V6_2C, PureContext.makeString_V6_11C)) {
      eval(s""" ${f.name}(["cat", "dog", "pig"], ", ") """) shouldBe CONST_STRING("cat, dog, pig")
      eval(s""" ${f.name}([], ", ") """) shouldBe CONST_STRING("")
      eval(s""" ${f.name}(["abc"], ", ") == "abc" """) shouldBe Right(CONST_BOOLEAN(true))
    }
  }

  property("makeString rejects non-string types in V6") {
    def mkConsList(values: List[EXPR]): EXPR = values match {
      case Nil =>
        REF(GlobalValNames.Nil)

      case value :: rest =>
        FUNCTION_CALL(
          Native(1100),
          List(value, mkConsList(rest))
        )
    }

    def script(funcId: Short): FUNCTION_CALL = FUNCTION_CALL(
      Native(funcId),
      List(
        mkConsList(List(CONST_STRING("test").explicitGet(), CONST_LONG(123), CONST_BOOLEAN(true), CONST_BYTESTR(ByteStr.empty).explicitGet())),
        CONST_STRING(",").explicitGet()
      )
    )

    evalExpr(script(FunctionIds.MAKESTRING), V4, V5, checkOldPowVersion = true) shouldBe Right(CONST_STRING("test,123,true,").explicitGet())
    evalExpr(script(FunctionIds.MAKESTRING), V6, V6) should produce("makeString only accepts strings")
    evalExpr(script(FunctionIds.MAKESTRING2C), V6, V6) should produce("makeString only accepts strings")
    evalExpr(script(FunctionIds.MAKESTRING11C), V6, V6) should produce("makeString only accepts strings")
  }

  property("makeString input limit") {
    Seq(V4, V5).foreach { version =>
      val script = s""" [${""" "a", """ * MaxListLengthV4} "a"].${PureContext.makeString.name}(", ") """
      eval(script)(version, checkNext = false) should produce(s"List size should not exceed $MaxListLengthV4")

      val script2 = s""" [${""" "a", """ * (MaxListLengthV4 - 1)} "a"].${PureContext.makeString.name}(", ") """
      eval(script2)(version, checkNext = false) shouldBe a[Right[?, ?]]
    }

    val script = s""" [${""" "a", """ * 70} "a"].${PureContext.makeString_V6.name}(", ") """
    eval(script)(V6) should produce("Input list size = 71 for makeString should not exceed 70")

    val script2 = s""" [${""" "a", """ * 69} "a"].${PureContext.makeString_V6.name}(", ") """
    eval(script2)(V6) shouldBe a[Right[?, ?]]
  }

  property("makeString output limit") {
    Seq(V4, V5).foreach { version =>
      val script = s""" [${s""" "${"a" * 1000}", """ * 32} "${"a" * 704}"].${PureContext.makeString.name}(", ") """
      eval(script)(version, checkNext = false) should produce("Constructing string size = 32768 bytes will exceed 32767")
      // 1000 * 32 + 704 + 2 * 32 = 32768

      val script2 = s""" [${s""" "${"a" * 1000}", """ * 32} "${"a" * 703}"].${PureContext.makeString.name}(", ") """
      eval(script2)(version, checkNext = false).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 32767
    }

    val script = s""" [${s""" "${"a" * 10}", """ * 25} "${"a" * 201}"].${PureContext.makeString_V6.name}(", ") """
    eval(script)(V6) should produce("Constructing string size = 501 bytes will exceed 500")
    // 10 * 25 + 201 + 2 * 25 = 501

    val script2 = s""" [${s""" "${"a" * 10}", """ * 25} "${"a" * 200}"].${PureContext.makeString_V6.name}(", ") """
    eval(script2)(V6).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 500
  }

  property("makeString function family input limit") {
    val script = s""" ${PureContext.makeString_V6_2C.name}([${s""" "${"a" * 5}", """ * 100} "a"], ", ") """
    eval(script)(V6) should produce(s"Input list size = 101 for ${PureContext.makeString_V6_2C.name} should not exceed 100")

    val script2 = s""" ${PureContext.makeString_V6_2C.name}([${s""" "${"a" * 5}", """ * 99} "a"], ", ") """
    eval(script2)(V6) shouldBe a[Right[?, ?]]

    val script3 = s""" ${PureContext.makeString_V6_11C.name}([${s""" "${"a" * 5}", """ * MaxListLengthV4} "a"], ", ") """
    eval(script3)(V6) should produce("List size should not exceed 1000")

    val script4 = s""" ${PureContext.makeString_V6_11C.name}([${s""" "${"a" * 5}", """ * (MaxListLengthV4 - 1)} "a"], ", ") """
    eval(script4)(V6) shouldBe a[Right[?, ?]]
  }

  property("makeString function family output limit") {
    val script = s""" ${PureContext.makeString_V6_2C.name}([${s""" "${"a" * 250}", """ * 20} "${"a" * 961}"], ", ") """
    eval(script)(V6) should produce("Constructing string size = 6001 bytes will exceed 6000")
    // 250 * 20 + 961 + 2 * 20 = 6001

    val script2 = s""" ${PureContext.makeString_V6_2C.name}([${s""" "${"a" * 250}", """ * 20} "${"a" * 960}"], ", ") """
    eval(script2)(V6).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 6000

    val script3 = s""" ${PureContext.makeString_V6_11C.name}([${s""" "${"a" * 1000}", """ * 32} "${"a" * 704}"], ", ") """
    eval(script3)(V6) should produce("Constructing string size = 32768 bytes will exceed 32767")
    // 10 * 42 + 1 + 2 * 42 = 505

    val script4 = s""" ${PureContext.makeString_V6_11C.name}([${s""" "${"a" * 1000}", """ * 32} "${"a" * 703}"], ", ") """
    eval(script4)(V6).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 32767
  }
}
