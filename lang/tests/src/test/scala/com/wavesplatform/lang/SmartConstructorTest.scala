package com.wavesplatform.lang

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_STRING}
import org.scalatest.{Inside, Matchers, PropSpec}

class SmartConstructorTest extends PropSpec with Matchers with Inside {
  private val dataTxBytesMax = 150 * 1024

  property("CONST_BYTESTR size limit") {
    val allowedBytes = ByteStr.fill(dataTxBytesMax)(1)
    inside(CONST_BYTESTR(allowedBytes)) {
      case Right(CONST_BYTESTR(bytes)) => bytes shouldBe allowedBytes
    }

    val illegalBytes = ByteStr.fill(dataTxBytesMax + 1)(1)
    CONST_BYTESTR(illegalBytes) shouldBe 'left
  }

  property("CONST_STRING size limit") {
    val allowedString = "ё" * (dataTxBytesMax / 2)
    inside(CONST_STRING(allowedString)) {
      case Right(CONST_STRING(str)) =>
        str shouldBe allowedString
        str.getBytes("UTF-8").length shouldBe dataTxBytesMax
    }

    val illegalString = "ё" * (dataTxBytesMax / 2 + 1)
    CONST_STRING(illegalString) shouldBe 'left
  }
}
