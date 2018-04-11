package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.TypeChecker.TypeCheckerContext
import com.wavesplatform.lang.ctx.impl.PureContext
import com.wavesplatform.lang.testing.ScriptGen
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
    forAll(BOOLgen(10)) { expr =>
      val typed = TypeChecker(TypeCheckerContext.fromContext(PureContext.instance), expr)
      typed shouldBe 'right
      roundtrip(Serde.codec, typed.right.get)
    }
  }
}
