package scorex.transaction.smart.script

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{FunctionHeaderType => FHT}
import com.wavesplatform.lang.v1.Terms.BOOLEAN
import com.wavesplatform.lang.v1.Terms.Typed._
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.transaction.smart.script.v1.ScriptV1

class ScriptV1Test extends PropSpec with PropertyChecks with Matchers with TypedScriptGen {

  property("ScriptV1.apply should permit BOOLEAN scripts") {
    forAll(BOOLEANgen(10)) { expr =>
      ScriptV1(expr) shouldBe 'right
    }
  }

  property("ScriptV1.apply should deny non-BOOLEAN scripts") {
    def invalidGen(gas: Int) = Gen.oneOf(LONGgen(gas), STRINGgen, BYTESTRgen)
    forAll(invalidGen(10)) { expr =>
      ScriptV1(expr) shouldBe Left("Script should return BOOLEAN")
    }
  }

  property("ScriptV1.apply should deny too complex scripts") {
    val byteVector = CONST_BYTEVECTOR(ByteVector(1))
    val expr = (1 to 21)
      .map { _ =>
        FUNCTION_CALL(
          function = FunctionHeader(name = "sigVerify", List(FHT.BYTEVECTOR, FHT.BYTEVECTOR, FHT.BYTEVECTOR)),
          args = List(byteVector, byteVector, byteVector),
          BOOLEAN
        ): EXPR
      }
      .reduceLeft(IF(_, _, FALSE, BOOLEAN))
    ScriptV1(expr) shouldBe Left("Script is too complex: 1890083 > 1800000")
  }

}
