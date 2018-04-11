package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._

object CostCalculator {
  val MaxCost = 1000

  def apply(t: Typed.EXPR): Int = t match {
    case Typed.CONST_LONG(_)                => 1
    case Typed.CONST_BYTEVECTOR(_)          => 1
    case Typed.IF(cond, ifTrue, ifFalse, _) => 2 + apply(cond) + Math.max(apply(ifTrue), apply(ifFalse))
    case _                                  => ???
  }
}
