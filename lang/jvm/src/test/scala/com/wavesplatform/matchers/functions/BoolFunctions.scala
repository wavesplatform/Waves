package com.wavesplatform.matchers.functions

import com.wavesplatform.matchers.ExprMatcher.{False, True}

trait BoolFunctions { self: PosMatcherFunctions =>
  val `true`  = True(anyPos)
  val `false` = False(anyPos)
}
