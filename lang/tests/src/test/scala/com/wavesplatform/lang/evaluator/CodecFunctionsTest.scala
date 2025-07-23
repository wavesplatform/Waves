package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.directives.values.V9
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64

import java.util

class CodecFunctionsTest extends EvaluatorSpec {
  private val kilobyte = {
    val bs = new Array[Byte](1024)
    util.Arrays.fill(bs, -1.toByte)
    bs
  }

  private val decodedKilobyte = CONST_BYTESTR(ByteStr(kilobyte)).explicitGet()
  private val base64Kilobyte = Base64.encode(kilobyte)

  property("fromBase16String_1С") {
    evalWithCost(s"fromBase16String_1C(\"${"FF"*1024}\")")(using V9) shouldBe (decodedKilobyte, 1)
    // mixed case is supported
    evalWithCost(s"fromBase16String_1C(\"${"fF"*1024}\")")(using V9) shouldBe (decodedKilobyte, 1)
    eval(s"fromBase16String_1C(\"${"FF"*1025}\")")(using V9) shouldBe Left("Base16 decode input length=2050 should not exceed 2048")
    // base16: prefix is not supported
    eval(s"fromBase16String_1C(\"base16:FFF\")")(using V9) shouldBe Left("Unrecognized character: s")
    eval(s"fromBase16String_1C(\"base16:FF\")")(using V9) shouldBe Left("Invalid input length 9")
  }

  property("toBase16String_1С") {
    // encoding is lowercase
    evalWithCost(s"toBase16String_1C(base16'${"FF" * 1024}')")(using V9) shouldBe (CONST_STRING("ff" * 1024).explicitGet(), 1)
    eval(s"toBase16String_1C(base16'${"FF" * 1025}')")(using V9) shouldBe Left("Base16 encode input length=1025 should not exceed 1024")
  }

  property("fromBase64String_1C") {
    evalWithCost(s"fromBase64String_1C(\"$base64Kilobyte\")")(using V9) shouldBe (decodedKilobyte, 1)
    // base64: prefix is stripped
    evalWithCost(s"fromBase64String_1C(\"base64:$base64Kilobyte\")")(using V9) shouldBe (decodedKilobyte, 1)
    // 1026 bytes fit in 1375-character limit
    eval(s"fromBase64String_1C(\"base64:${Base64.encode(new Array[Byte](1026))}\")")(using V9) shouldBe Left("byte vector length 1026 exceeds limit 1024")
    eval(s"fromBase64String_1C(\"base64:${Base64.encode(new Array[Byte](1027))}\")")(using V9) shouldBe Left("base64Decode input exceeds 1375")
  }

  property("toBase64String_1C") {
    evalWithCost(s"toBase64String_1C(base64'$base64Kilobyte')")(using V9) shouldBe (CONST_STRING(base64Kilobyte).explicitGet(), 1)
    // base64: prefix is stripped
    evalWithCost(s"toBase64String_1C(base64'base64:$base64Kilobyte')")(using V9) shouldBe (CONST_STRING(base64Kilobyte).explicitGet(), 1)
    eval(s"toBase64String_1C(base64'${Base64.encode(new Array[Byte](1025))}')")(using V9) shouldBe Left("base64Encode input exceeds 1024")
  }
}
