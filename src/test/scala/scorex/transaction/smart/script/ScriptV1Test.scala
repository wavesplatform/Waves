package scorex.transaction.smart.script

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Terms.{BOOLEAN, LONG}
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.state.diffs.produce
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
          function = FunctionHeader(name = "sigVerify"),
          args = List(byteVector, byteVector, byteVector),
          BOOLEAN
        )
      }
      .reduceLeft[EXPR](IF(_, _, FALSE, BOOLEAN))

    ScriptV1(expr) should produce("Script is too complex")
  }

  property("ScriptV1.apply should deny too big scripts") {
    val bigSum = (1 to 100).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
      FUNCTION_CALL(
        function = FunctionHeader(name = "l=l"),
        args = List(r, CONST_LONG(i)),
        LONG
      )
    }
    val expr = (1 to 9).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
      FUNCTION_CALL(
        function = FunctionHeader(name = "l=l"),
        args = List(r, bigSum),
        BOOLEAN
      )
    }

    ScriptV1(expr) should produce("Script is too large")
  }

  property("19 sigVerify should fit in maxSizeInBytes") {
    val byteVector = CONST_BYTEVECTOR(ByteVector(1))
    val expr = (1 to 19)
      .map { _ =>
        FUNCTION_CALL(
          function = FunctionHeader(name = "sigVerify"),
          args = List(byteVector, byteVector, byteVector),
          BOOLEAN
        )
      }
      .reduceLeft[EXPR](IF(_, _, FALSE, BOOLEAN))

    ScriptV1(expr) shouldBe 'right
  }

}
