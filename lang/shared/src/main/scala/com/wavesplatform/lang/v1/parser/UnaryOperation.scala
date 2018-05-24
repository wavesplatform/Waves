package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions._
import fastparse.all._

object UnaryOperation {
  val unaryOps = List(
    P("-" ~ !CharIn('0' to '9')) -> { e: EXPR =>
      FUNCTION_CALL("-", List(e))
    },
    P("!") -> { e: EXPR =>
      FUNCTION_CALL("!", List(e))
    }
  )
}
