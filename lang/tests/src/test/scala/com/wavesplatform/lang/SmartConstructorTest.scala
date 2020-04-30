package com.wavesplatform.lang

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_STRING}
import org.scalatest.{Inside, Matchers, PropSpec}

class SmartConstructorTest extends PropSpec with Matchers with Inside {
  property("CONST_BYTESTR size limit") {
    val allowedBytes = ByteStr.fill(Terms.DATA_ENTRY_VALUE_MAX)(1)
    inside(CONST_BYTESTR(allowedBytes)) {
      case Right(CONST_BYTESTR(bytes)) => bytes shouldBe allowedBytes
    }

    val illegalBytes = ByteStr.fill(Terms.DATA_ENTRY_VALUE_MAX + 1)(1)
    CONST_BYTESTR(illegalBytes) shouldBe 'left
  }

  property("CONST_STRING size limit") {
    val allowedString = "ё" * (Terms.DATA_ENTRY_VALUE_MAX / 2)
    inside(CONST_STRING(allowedString)) {
      case Right(CONST_STRING(str)) =>
        str shouldBe allowedString
        str.getBytes("UTF-8").length shouldBe Terms.DATA_ENTRY_VALUE_MAX - 1
    }

    val illegalString = "ё" * (Terms.DATA_ENTRY_VALUE_MAX / 2 + 1)
    CONST_STRING(illegalString) shouldBe 'left
  }
}
