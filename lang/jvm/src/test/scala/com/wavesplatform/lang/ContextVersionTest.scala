package com.wavesplatform.lang

import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import org.scalatest.{FreeSpec, Matchers}

class ContextVersionTest extends FreeSpec with Matchers {

  "InvokeScriptTransaction" - {
    "exist in lib version 3" in {
      val types = Types.buildWavesTypes(true, StdLibVersion.V3)
      types.count(c => c.name == "InvokeScriptTransaction") shouldEqual 1
    }

    "doesn't exist in lib version 2" in {
      val types = Types.buildWavesTypes(true, StdLibVersion.V2)
      types.count(c => c.name == "InvokeScriptTransaction") shouldEqual 0
    }
  }
}
