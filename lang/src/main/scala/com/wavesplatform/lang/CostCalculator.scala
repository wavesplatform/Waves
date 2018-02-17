package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._

object CostCalculator {
  val MaxCost = 1000

  def apply(t: Typed.EXPR): Int = t match {
    case Typed.CONST_INT(_)                 => 1
    case Typed.CONST_BYTEVECTOR(_)          => 1
    case Typed.SUM(i1, i2)                  => 2 + apply(i1) + apply(i2)
    case Typed.AND(t1, t2)                  => 2 + apply(t1) + apply(t2)
    case Typed.OR(t1, t2)                   => 2 + apply(t1) + apply(t2)
    case Typed.EQ(t1, t2)                   => 2 + apply(t1) + apply(t2)
    case Typed.GT(t1, t2)                   => 2 + apply(t1) + apply(t2)
    case Typed.GE(t1, t2)                   => 2 + apply(t1) + apply(t2)
    case Typed.IF(cond, ifTrue, ifFalse, _) => 2 + apply(cond) + Math.max(apply(ifTrue), apply(ifFalse))
    case Typed.SIG_VERIFY(_, _, _)          => 100
    case _                                  => ???
  }
}
