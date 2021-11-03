package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.{V1, V5, V6}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.test._

class BytesDropTakeTest extends EvaluatorSpec {
  private val min   = Long.MinValue
  private val max   = Long.MaxValue
  private val limit = 153600

  private val b = s"base58'${ByteStr.fromBytes(1, 2, 3, 4, 5)}'"

  private def bytes(bytes: Byte*) = CONST_BYTESTR(ByteStr.fromBytes(bytes: _*))

  property("take") {
    eval(s""" take($b, 0) """) shouldBe bytes()
    eval(s""" take($b, 2) """) shouldBe bytes(1, 2)
    eval(s""" take($b, 100) """) shouldBe bytes(1, 2, 3, 4, 5)

    eval(s""" take($b, $max) """, V1, V5) shouldBe bytes()
    eval(s""" take($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to take() exceeds ByteVector limit = $limit")
    eval(s""" take($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to take() exceeds ByteVector limit = $limit")

    eval(s""" take($b, -1) """, V1, V5) shouldBe bytes()
    eval(s""" take($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to take()")
    eval(s""" take($b, $min) """, V1, V5) shouldBe bytes()
    eval(s""" take($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to take()")
  }

  property("takeRight") {
    eval(s""" takeRight($b, 0) """) shouldBe bytes()
    eval(s""" takeRight($b, 2) """) shouldBe bytes(4, 5)
    eval(s""" takeRight($b, 100) """) shouldBe bytes(1, 2, 3, 4, 5)

    eval(s""" takeRight($b, $max) """, V1, V5) shouldBe bytes()
    eval(s""" takeRight($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to takeRight() exceeds ByteVector limit = $limit")
    eval(s""" takeRight($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to takeRight() exceeds ByteVector limit = $limit")

    eval(s""" takeRight($b, -1) """, V1, V5) shouldBe bytes()
    eval(s""" takeRight($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to takeRight()")
    eval(s""" takeRight($b, $min) """, V1, V5) shouldBe Left("long overflow")
    eval(s""" takeRight($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to takeRight()")
  }

  property("drop") {
    eval(s""" drop($b, 0) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, 2) """) shouldBe bytes(3, 4, 5)
    eval(s""" drop($b, 100) """) shouldBe bytes()

    eval(s""" drop($b, $max) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to drop() exceeds ByteVector limit = $limit")
    eval(s""" drop($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to drop() exceeds ByteVector limit = $limit")

    eval(s""" drop($b, -1) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to drop()")
    eval(s""" drop($b, $min) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to drop()")
  }

  property("dropRight") {
    eval(s""" dropRight($b, 0) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, 2) """) shouldBe bytes(1, 2, 3)
    eval(s""" dropRight($b, 100) """) shouldBe bytes()

    eval(s""" dropRight($b, $max) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to dropRight() exceeds ByteVector limit = $limit")
    eval(s""" dropRight($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to dropRight() exceeds ByteVector limit = $limit")

    eval(s""" dropRight($b, -1) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to dropRight()")
    eval(s""" dropRight($b, $min) """, V1, V5) should produce("long overflow")
    eval(s""" dropRight($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to dropRight()")
  }
}
