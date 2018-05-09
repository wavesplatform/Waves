package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.testing.ScriptGen
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
      val typed = CompilerV1(CompilerContext.fromExecutionContext(PureContext.instance), expr)
      typed shouldBe 'right
      roundtrip(Serde.codec, typed.right.get)
    }
  }
}
