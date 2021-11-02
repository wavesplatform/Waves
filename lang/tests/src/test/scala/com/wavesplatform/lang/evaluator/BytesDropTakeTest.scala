package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.{V1, V5, V6}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.test._

class BytesDropTakeTest extends EvaluatorSpec {
  private def bytes(bytes: Byte*) = CONST_BYTESTR(ByteStr.fromBytes(bytes: _*))

  private val b   = s"base58'${ByteStr.fromBytes(1, 2, 3, 4, 5)}'"
  private val min = Long.MinValue
  private val max = Long.MaxValue

  property("take") {
    eval(s""" take($b, 0) """) shouldBe bytes()
    eval(s""" take($b, 2) """) shouldBe bytes(1, 2)

    eval(s""" take($b, 100) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" take($b, $max) """, V1, V5) shouldBe bytes()
    eval(s""" take($b, $max) """)(V6) shouldBe bytes(1, 2, 3, 4, 5)

    eval(s""" take($b, -100) """) shouldBe bytes()
    eval(s""" take($b, $min) """) shouldBe bytes()
  }

  property("takeRight") {
    eval(s""" takeRight($b, 0) """) shouldBe bytes()
    eval(s""" takeRight($b, 2) """) shouldBe bytes(4, 5)

    eval(s""" takeRight($b, 100) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" takeRight($b, $max) """, V1, V5) shouldBe bytes()
    eval(s""" takeRight($b, $max) """)(V6) shouldBe bytes(1, 2, 3, 4, 5)

    eval(s""" takeRight($b, -100) """) shouldBe bytes()
    eval(s""" takeRight($b, $min) """, V1, V5) should produce("long overflow")
    eval(s""" takeRight($b, $min) """)(V6) shouldBe bytes()
  }

  property("drop") {
    eval(s""" drop($b, 0) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, 2) """) shouldBe bytes(3, 4, 5)

    eval(s""" drop($b, 100) """) shouldBe bytes()
    eval(s""" drop($b, $max) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, $max) """)(V6) shouldBe bytes()

    eval(s""" drop($b, -100) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, $min) """) shouldBe bytes(1, 2, 3, 4, 5)
  }

  property("dropRight") {
    eval(s""" dropRight($b, 0) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, 2) """) shouldBe bytes(1, 2, 3)

    eval(s""" dropRight($b, 100) """) shouldBe bytes()
    eval(s""" dropRight($b, $max) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, $max) """)(V6) shouldBe bytes()

    eval(s""" dropRight($b, -100) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, $min) """, V1, V5) should produce("long overflow")
    eval(s""" dropRight($b, $min) """)(V6) shouldBe bytes(1, 2, 3, 4, 5)
  }
}
