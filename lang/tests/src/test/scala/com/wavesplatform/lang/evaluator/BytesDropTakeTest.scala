package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.{V1, V5, V6}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.*
import com.wavesplatform.test.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR.NoLimit

class BytesDropTakeTest extends EvaluatorSpec {
  private val min   = Long.MinValue
  private val max   = Long.MaxValue
  private val limit = 165947

  private val b = s"base58'${ByteStr.fromBytes(1, 2, 3, 4, 5)}'"

  private def bytes(bytes: Byte*) = CONST_BYTESTR(ByteStr.fromBytes(bytes *))

  property("take") {
    eval(s""" take($b, 0) """) shouldBe bytes()
    eval(s""" take($b, 2) """) shouldBe bytes(1, 2)
    eval(s""" take($b, $limit) """) shouldBe bytes(1, 2, 3, 4, 5)

    evalVerRange(s""" take($b, $max) """, V1, V5) shouldBe bytes()
    eval(s""" take($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to take() exceeds ByteVector limit = $limit")
    eval(s""" take($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to take() exceeds ByteVector limit = $limit")

    evalVerRange(s""" take($b, -1) """, V1, V5) shouldBe bytes()
    eval(s""" take($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to take()")
    evalVerRange(s""" take($b, $min) """, V1, V5) shouldBe bytes()
    eval(s""" take($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to take()")
  }

  property("takeRight") {
    eval(s""" takeRight($b, 0) """) shouldBe bytes()
    eval(s""" takeRight($b, 2) """) shouldBe bytes(4, 5)
    eval(s""" takeRight($b, $limit) """) shouldBe bytes(1, 2, 3, 4, 5)

    evalVerRange(s""" takeRight($b, $max) """, V1, V5) shouldBe bytes()
    eval(s""" takeRight($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to takeRight() exceeds ByteVector limit = $limit")
    eval(s""" takeRight($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to takeRight() exceeds ByteVector limit = $limit")

    evalVerRange(s""" takeRight($b, -1) """, V1, V5) shouldBe bytes()
    eval(s""" takeRight($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to takeRight()")
    evalVerRange(s""" takeRight($b, $min) """, V1, V5) shouldBe Left("long overflow")
    eval(s""" takeRight($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to takeRight()")
  }

  property("drop") {
    eval(s""" drop($b, 0) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, 2) """) shouldBe bytes(3, 4, 5)
    eval(s""" drop($b, $limit) """) shouldBe bytes()

    evalVerRange(s""" drop($b, $max) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to drop() exceeds ByteVector limit = $limit")
    eval(s""" drop($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to drop() exceeds ByteVector limit = $limit")

    evalVerRange(s""" drop($b, -1) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to drop()")
    evalVerRange(s""" drop($b, $min) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" drop($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to drop()")
  }

  property("dropRight") {
    eval(s""" dropRight($b, 0) """) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, 2) """) shouldBe bytes(1, 2, 3)
    eval(s""" dropRight($b, $limit) """) shouldBe bytes()

    evalVerRange(s""" dropRight($b, $max) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, $max) """)(V6) shouldBe Left(s"Number = $max passed to dropRight() exceeds ByteVector limit = $limit")
    eval(s""" dropRight($b, ${limit + 1}) """)(V6) shouldBe Left(s"Number = ${limit + 1} passed to dropRight() exceeds ByteVector limit = $limit")

    evalVerRange(s""" dropRight($b, -1) """, V1, V5) shouldBe bytes(1, 2, 3, 4, 5)
    eval(s""" dropRight($b, -1) """)(V6) shouldBe Left("Unexpected negative number = -1 passed to dropRight()")
    evalVerRange(s""" dropRight($b, $min) """, V1, V5) should produce("long overflow")
    eval(s""" dropRight($b, $min) """)(V6) shouldBe Left(s"Unexpected negative number = $min passed to dropRight()")
  }

  property("max size bytes") {
    val maxBytes        = CONST_BYTESTR(ByteStr.fill(limit)(1), NoLimit)
    def call(id: Short) = FUNCTION_CALL(Native(id), List(maxBytes.explicitGet(), CONST_LONG(limit)))
    evalExpr(call(TAKE_BYTES), V6, lastVersion) shouldBe maxBytes
    evalExpr(call(TAKE_RIGHT_BYTES), V6, lastVersion) shouldBe maxBytes
    evalExpr(call(DROP_BYTES), V6, lastVersion) shouldBe CONST_BYTESTR(ByteStr.empty)
    evalExpr(call(DROP_RIGHT_BYTES), V6, lastVersion) shouldBe CONST_BYTESTR(ByteStr.empty)
  }
}
