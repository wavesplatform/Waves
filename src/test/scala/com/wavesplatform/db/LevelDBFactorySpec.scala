package com.wavesplatform.db

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class LevelDBFactorySpec extends FreeSpec with Matchers with PropertyChecks {
  "Load DBFactory implementation" in {
    LevelDBFactory.factory.getClass.getName.endsWith("DBFactory") shouldBe true
  }
}
