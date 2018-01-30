package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._

object CostCalculator {
  val MaxCost = 1000

  def apply(t: Expr): Int = t match {
    case CONST_INT(_)              => 1
    case HEIGHT                    => 1
    case CONST_BYTEVECTOR(_)       => 1
    case TX_FIELD(f)               => 2
    case SUM(i1, i2)               => 2 + apply(i1) + apply(i2)
    case AND(t1, t2)               => 2 + apply(t1) + apply(t2)
    case OR(t1, t2)                => 2 + apply(t1) + apply(t2)
    case EQ_INT(t1, t2)            => 2 + apply(t1) + apply(t2)
    case GT(t1, t2)                => 2 + apply(t1) + apply(t2)
    case GE(t1, t2)                => 2 + apply(t1) + apply(t2)
    case IF(cond, ifTrue, ifFalse) => 2 + apply(cond) + Math.max(apply(ifTrue), apply(ifFalse))
    case SIG_VERIFY(_, _, _)       => 100
  }
}
