package com.wavesplatform.lang.evaluator.math

import cats.Id
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.evaluator.ContextfulNativeFunction
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, NativeFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.test.PropSpec

class LongOverflowMessageTest extends PropSpec {
  // JDK 22+ C2 throws a preallocated ArithmeticException without a message from hot Math.*Exact overflows; a few thousand calls get there.
  private val calls = 20000

  private def errorMessages(function: BaseFunction[?], a: Long, b: Long): Set[String] = {
    val ev   = function.asInstanceOf[NativeFunction[[_[_]] =>> Any]].ev.asInstanceOf[ContextfulNativeFunction.Simple[[_[_]] =>> Any]]
    val args = List(CONST_LONG(a), CONST_LONG(b))
    (1 to calls).map(_ => ev.evaluate[Id]((), args).left.map(_.message).swap.toOption.orNull).toSet
  }

  property("long overflow keeps its message however hot the operator is") {
    errorMessages(PureContext.sumLong, Long.MaxValue, 1) shouldBe Set("long overflow")
    errorMessages(PureContext.subLong, Long.MinValue, 1) shouldBe Set("long overflow")
    errorMessages(PureContext.mulLong, Long.MaxValue, 2) shouldBe Set("long overflow")
  }
}
