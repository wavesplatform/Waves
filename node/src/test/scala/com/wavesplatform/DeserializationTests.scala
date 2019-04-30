package com.wavesplatform

import com.wavesplatform.serialization.Deser
import org.scalatest.{FreeSpec, Matchers}

class DeserializationTests extends FreeSpec with Matchers {

  "serializeArray" - {
    "works with arrays < 32k" in {
      val byteArray = Array.fill(Short.MaxValue)(0.toByte)
      Deser.serializeArray(byteArray) should not be empty
    }
    "IllegalArgumentException thrown with arrays > 32k" in {
      val byteArray = Array.fill(Short.MaxValue + 1)(0.toByte)
      an[IllegalArgumentException] should be thrownBy Deser.serializeArray(byteArray)
    }
  }
}
