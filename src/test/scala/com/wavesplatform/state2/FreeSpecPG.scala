package com.wavesplatform.state2

import org.scalatest.{FreeSpec, Matchers}

class FreeSpecPG extends FreeSpec with Matchers {
  "sdf" - {
    Range(0,10).foreach { n =>
      s"$n - testing" in {
        if (n>5) throw new Exception()
      }
      s"$n - passing" in {
      }
    }
  }
}
