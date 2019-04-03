package com.wavesplatform.lang

import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import org.scalatest.{FreeSpec, Matchers}

class TypesTest extends FreeSpec with Matchers {

  "lib version 3" - {
    val types = Types.buildWavesTypes(true, StdLibVersion.V3)

    "there should be only one InvokeScriptTransaction" in {
      types.count(c => c.name == "InvokeScriptTransaction") shouldEqual 1
    }
  }

  "lib version 2" - {
    val types = Types.buildWavesTypes(true, StdLibVersion.V2)

    "there should be no InvokeScriptTransaction" in {
      types.count(c => c.name == "InvokeScriptTransaction") shouldEqual 0
    }
  }
}
