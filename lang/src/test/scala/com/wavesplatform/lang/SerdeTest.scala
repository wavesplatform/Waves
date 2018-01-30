package com.wavesplatform.lang

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult}

class SerdeTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  protected def roundtrip[A](codec: Codec[A], value: A): Unit = {
    val encoded = codec.encode(value)
    encoded.isSuccessful shouldBe true
    val Attempt.Successful(DecodeResult(decoded, remainder)) = codec.decode(encoded.require)
    remainder shouldEqual BitVector.empty
    decoded shouldEqual value
    ()
  }

  property("Script roundtrip") {
    forAll(BOOLgen(500)) { roundtrip(Serde.codec, _) }
  }
}
