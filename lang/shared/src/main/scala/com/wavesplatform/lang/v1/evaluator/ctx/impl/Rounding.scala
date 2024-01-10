package com.wavesplatform.lang.v1.evaluator.ctx.impl
import java.math.RoundingMode
import java.math.RoundingMode.*

import com.wavesplatform.lang.v1.compiler.Terms.{CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ContextfulVal

sealed abstract class Rounding(typeName: String, val mode: RoundingMode) {
  val `type`: CASETYPEREF                                = CASETYPEREF(typeName, Nil, hideConstructor = true)
  val value: CaseObj                                     = CaseObj(`type`, Map())
  val definition: (String, (CASETYPEREF, ContextfulVal)) = (typeName.toUpperCase, (`type`, ContextfulVal.pure(value)))
}

case object Rounding {
  case object Down     extends Rounding("Down", DOWN)
  case object Up       extends Rounding("Up", UP)
  case object HalfDown extends Rounding("HalfDown", HALF_DOWN)
  case object HalfUp   extends Rounding("HalfUp", HALF_UP)
  case object HalfEven extends Rounding("HalfEven", HALF_EVEN)
  case object Ceiling  extends Rounding("Ceiling", CEILING)
  case object Floor    extends Rounding("Floor", FLOOR)

  val fromV5: List[Rounding]            = List(Down, HalfUp, HalfEven, Ceiling, Floor)
  val all: List[Rounding]               = List(Up, HalfDown) ::: fromV5
  val byValue: Map[EVALUATED, Rounding] = all.map(r => (r.value: EVALUATED, r)).toMap
}
