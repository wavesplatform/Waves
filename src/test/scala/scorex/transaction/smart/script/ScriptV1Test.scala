package scorex.transaction.smart.script

import com.wavesplatform.lang.v1.testing.TypedScriptGen
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
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

}
