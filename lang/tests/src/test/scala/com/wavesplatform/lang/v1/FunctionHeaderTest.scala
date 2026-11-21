package com.wavesplatform.lang.v1

import com.wavesplatform.test.PropSpec

class FunctionHeaderTest extends PropSpec {
  // What the compiler generates for a case class of that shape and name, the hash Native had before it was cached.
  private case class Native(name: Short)

  property("a native header hashes as the case class did, so maps keyed by headers keep their order") {
    (Short.MinValue.toInt to Short.MaxValue.toInt).foreach { i =>
      FunctionHeader.Native(i.toShort).hashCode shouldBe Native(i.toShort).hashCode
    }
  }
}
