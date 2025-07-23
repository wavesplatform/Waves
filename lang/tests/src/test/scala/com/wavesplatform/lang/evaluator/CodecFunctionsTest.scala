package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.test.*

import java.util

class CodecFunctionsTest extends FreeSpec, EvaluatorSpecBase {
  private val kilobyte = {
    val bs = new Array[Byte](1024)
    util.Arrays.fill(bs, -1.toByte)
    bs
  }

  private val decodedKilobyte = CONST_BYTESTR(ByteStr(kilobyte)).explicitGet()
  private val base64Kilobyte  = Base64.encode(kilobyte)
  private val eightKb         = s"base64'${Base64.encode(new Array[Byte](8 * 1024))}'"

  "fromBase16String_1С" - {
    "input string size is checked" in {
      evalWithCost(s"fromBase16String_1C(\"${"FF" * 1024}\")")(using V9) shouldBe (decodedKilobyte, 1)
      eval(s"fromBase16String_1C(\"${"FF" * 1025}\")")(using V9) shouldBe Left("Base16 decode input length=2050 should not exceed 2048")
    }
    "mixed case is supported" in {
      evalWithCost(s"fromBase16String_1C(\"${"fF" * 1024}\")")(using V9) shouldBe (decodedKilobyte, 1)
    }
    "base16: prefix is not supported" in {
      eval(s"fromBase16String_1C(\"base16:FFF\")")(using V9) shouldBe Left("Unrecognized character: s")
      eval(s"fromBase16String_1C(\"base16:FF\")")(using V9) shouldBe Left("Invalid input length 9")
    }
  }

  "toBase16String_1С" - {
    "encoding is lowercase" in {
      evalWithCost(s"toBase16String_1C(base16'${"FF" * 1024}')")(using V9) shouldBe (CONST_STRING("ff" * 1024).explicitGet(), 1)
    }
    "input byte vector size is checked" in {
      eval(s"toBase16String_1C(base16'${"FF" * 1025}')")(using V9) shouldBe Left("Base16 encode input length=1025 should not exceed 1024")
    }
  }

  "fromBase64String_1C" - {
    "input string size is checked" in {
      evalWithCost(s"fromBase64String_1C(\"$base64Kilobyte\")")(using V9) shouldBe (decodedKilobyte, 1)
      eval(s"fromBase64String_1C(\"base64:${Base64.encode(new Array[Byte](1027))}\")")(using V9) shouldBe Left("base64Decode input exceeds 1375")
    }
    "result byte vector size is checked" in {
      eval(s"fromBase64String_1C(\"${Base64.encode(new Array[Byte](1026))}\")")(using V9) shouldBe Left(
        "byte vector length 1026 exceeds limit 1024"
      )
    }
    "base64: prefix is stripped" in {
      evalWithCost(s"fromBase64String_1C(\"base64:$base64Kilobyte\")")(using V9) shouldBe (decodedKilobyte, 1)
    }
  }

  "toBase64String_1C" - {
    "input byte vector size is checked" in {
      evalWithCost(s"toBase64String_1C(base64'$base64Kilobyte')")(using V9) shouldBe (CONST_STRING(base64Kilobyte).explicitGet(), 1)
      eval(s"toBase64String_1C(base64'${Base64.encode(new Array[Byte](1025))}')")(using V9) shouldBe Left("base64Encode input exceeds 1024")
    }
  }

  "fromBase58String" - {
    "input string size is checked" in {
      eval(s"fromBase58String(\"${"1" * 100}\")").explicitGet() shouldBe CONST_BYTESTR(ByteStr(new Array[Byte](100))).explicitGet()
      eval(s"fromBase58String(\"${"A" * 101}\")") shouldBe Left("base58Decode input exceeds 100")
    }

    "base58: prefix is not supported" in {
      eval(s"fromBase58String(\"base58:AAA\")") shouldBe Left("can't parse Base58 string")
    }
  }

  "toBase58String" - {
    "input byte vector size is checked" in {
      eval(s"toBase58String(base64'${Base64.encode(new Array[Byte](64))}')").explicitGet() shouldBe CONST_STRING("1" * 64).explicitGet()
      eval(s"toBase58String(base64'${Base64.encode(new Array[Byte](65))}')") shouldBe Left("base58Encode input exceeds 64")
    }
  }

  "fromBase64String" - {
    "input string size is checked" in {
      evalExpr(
        FUNCTION_CALL(
          Native(FunctionIds.FROMBASE64),
          List(CONST_STRING("A" * (44 * 1024 + 1), false).explicitGet())
        ),
        V4,
        V9
      ) shouldBe Left("base64Decode input exceeds 45056")
    }
    "result byte vector size is checked" in {
      evalExpr(
        FUNCTION_CALL(
          Native(FunctionIds.FROMBASE64),
          List(CONST_STRING(Base64.encode(new Array[Byte](32 * 1024)), false).explicitGet())
        ),
        V4,
        V9
      ) shouldBe Left("ByteStr size=32768 exceeds 32767 bytes")
    }
    "base64: prefix is stripped" in {
      eval(s"fromBase64String(\"base64:$base64Kilobyte\")").explicitGet() shouldBe decodedKilobyte
    }
  }

  "toBase64String" - {
    "input byte vector size is checked" in {
      evalExpr(
        FUNCTION_CALL(
          Native(FunctionIds.TOBASE64),
          List(CONST_BYTESTR(ByteStr(new Array[Byte](32 * 1024 + 1)), CONST_BYTESTR.DataTxSize).explicitGet())
        ),
        V4,
        V9
      ) shouldBe Left("base64Encode input exceeds 32768")
    }
  }

  "fromBase16String" - {
    "input string size is checked" in {
      evalExpr(
        FUNCTION_CALL(
          Native(FunctionIds.FROMBASE16),
          List(CONST_STRING("FF" * (16 * 1024 + 1), false).explicitGet())
        ),
        V4,
        V9
      ) shouldBe Left("Base16 decode input length=32770 should not exceed 32768")
    }
    "mixed case is supported" in {
      eval(s"fromBase16String(\"${"fF" * 1024}\")")(using V3).explicitGet() shouldBe decodedKilobyte
    }
  }

  "toBase16String" - {
    "encoding is lowercase" in {
      eval(s"toBase16String(base16'${"FF" * 1024}')")(using V3).explicitGet() shouldBe CONST_STRING("ff" * 1024).explicitGet()
    }
    "input byte vector size is checked" in {
      eval(s"toBase16String($eightKb + $eightKb + $eightKb)")(using V4) shouldBe Left("Base16 encode input length=24576 should not exceed 8192")
    }
  }
}
