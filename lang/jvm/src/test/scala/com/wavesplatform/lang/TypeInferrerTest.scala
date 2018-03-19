package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import org.scalatest.{FreeSpec, Matchers}
import Common._

class TypeInferrerTest extends FreeSpec with Matchers {

  // (ACTUAL, EXPECTED)

  "no types to infer" - {
    "all types are correct" in {
      TypeInferrer(Seq((LONG, LONG), (LONG, LONG), (TYPEREF("User"), TYPEREF("User")))) shouldBe Right(Map.empty)
    }
    "no simple common type" in {
      TypeInferrer(Seq((LONG, BYTEVECTOR))) should produce("Non-matching types")
    }

    "no obj common type" in {
      TypeInferrer(Seq((TYPEREF("User"), TYPEREF("Admin")))) should produce("Non-matching types")
    }
  }

  "inferring" - {
    "simple type" in {
      TypeInferrer(Seq((LONG, TYPEPARAM('T')))) shouldBe Right(Map(TYPEPARAM('T') -> LONG))
    }

    "many simple types" in {
      TypeInferrer(
        Seq(
          (LONG, TYPEPARAM('T')),
          (BYTEVECTOR, TYPEPARAM('G'))
        )) shouldBe Right(Map(TYPEPARAM('T') -> LONG, TYPEPARAM('G') -> BYTEVECTOR))
    }

    "option" - {
      "as plain type" in {
        TypeInferrer(Seq((OPTION(LONG), TYPEPARAM('T')))) shouldBe Right(Map(TYPEPARAM('T') -> OPTION(LONG)))
      }

      "as inner type" in {
        TypeInferrer(Seq((OPTION(LONG), OPTIONTYPEPARAM(TYPEPARAM('T'))))) shouldBe Right(Map(TYPEPARAM('T') -> LONG))
      }
    }
  }
}
