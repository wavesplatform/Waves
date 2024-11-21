package com.wavesplatform.lang

import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesplatform.test.FreeSpec

class ContextVersionTest extends FreeSpec {

  "InvokeScriptTransaction" - {
    "exist in lib version 3" in {
      val types = Types.buildWavesTypes(true, V3)
      types.count(c => c.name == "InvokeScriptTransaction") shouldEqual 1
    }

    "doesn't exist in lib version 2" in {
      val types = Types.buildWavesTypes(true, V2)
      types.count(c => c.name == "InvokeScriptTransaction") shouldEqual 0
    }
  }
}
