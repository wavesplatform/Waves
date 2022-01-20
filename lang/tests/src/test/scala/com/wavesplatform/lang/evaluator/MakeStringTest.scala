package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V6}
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, EXPR, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.test.produce

class MakeStringTest extends EvaluatorSpec {
  implicit val v: StdLibVersion = V6

  property("correct results") {
    for (f <- List(PureContext.makeString, PureContext.makeString_V6_1C, PureContext.makeString_V6_2C)) {
      eval(s""" ${f.name}(["cat", "dog", "pig"], ", ") """) shouldBe CONST_STRING("cat, dog, pig")
      eval(s""" ${f.name}([], ", ") """) shouldBe CONST_STRING("")
      eval(s""" ${f.name}(["abc"], ", ") == "abc" """) shouldBe Right(CONST_BOOLEAN(true))
    }
  }

  property("makeString rejects non-string types in V6") {
    def mkConsList(values: List[EXPR]): EXPR = values match {
      case Nil =>
        REF("nil")

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
    
    eval(script(FunctionIds.MAKESTRING), V4, V5) shouldBe Right(CONST_STRING("test,123,true,").explicitGet())
    eval(script(FunctionIds.MAKESTRING), V6, V6) should produce("makeString only accepts strings")
    eval(script(FunctionIds.MAKESTRING1C), V6, V6) should produce("makeString only accepts strings")
    eval(script(FunctionIds.MAKESTRING2C), V6, V6) should produce("makeString only accepts strings")
  }

  property("makeString limit") {
    implicit val v: StdLibVersion = V4

    val script = s""" [${s""" "${"a" * 1000}", """ * 32} "${"a" * 704}"].makeString(", ") """
    eval(script) should produce("Constructing string size = 32768 bytes will exceed 32767")
    // 1000 * 32 + 704 + 2 * 32 = 32768

    val script2 = s""" [${s""" "${"a" * 1000}", """ * 32} "${"a" * 703}"].makeString(", ") """
    eval(script2).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 32767
  }

  property("makeString function family input limit") {
    for ((f, limit) <- List((PureContext.makeString_V6_1C, 70), (PureContext.makeString_V6_2C, 100))) {
      val script = s""" ${f.name}([${s""" "${"a" * 5}", """ * limit} "a"], ", ") """
      eval(script) should produce(s"Input list size = ${limit + 1} for ${f.name} should not exceed $limit")

      val script2 = s""" ${f.name}([${s""" "${"a" * 5}", """ * (limit - 1)} "a"], ", ") """
      eval(script2) shouldBe a[Right[_, _]]
    }
  }

  property("makeString_1C output limit") {
    val script = s""" makeString_1C([${s""" "${"a" * 10}", """ * 42} "a"], ", ") """
    eval(script) should produce("Constructing string size = 505 bytes will exceed 500")
    // 10 * 42 + 1 + 2 * 42 = 505

    val script2 = s""" makeString_1C([${s""" "${"a" * 10}", """ * 41} "a"], ", ") """
    eval(script2).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 493
  }

  property("makeString_2C output limit") {
    val script = s""" makeString_2C([${s""" "${"a" * 59}", """ * 99} "a"], ", ") """
    eval(script) should produce("Constructing string size = 6040 bytes will exceed 6000")
    // 59 * 99 + 1 + 2 * 99 = 6040

    val script2 = s""" makeString_2C([${s""" "${"a" * 58}", """ * 99} "a"], ", ") """
    eval(script2).explicitGet().asInstanceOf[CONST_STRING].s.length shouldBe 5941
  }
}
