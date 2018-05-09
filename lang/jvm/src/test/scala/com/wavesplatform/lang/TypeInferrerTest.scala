package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.Terms._
import org.scalatest.{FreeSpec, Matchers}
import Common._
import com.wavesplatform.lang.v1.TypeInferrer

class TypeInferrerTest extends FreeSpec with Matchers {

  val typeparamT = TYPEPARAM('T')
  val typeparamG = TYPEPARAM('G')

  "no types to infer" - {
    "all types are correct" in {
      TypeInferrer(Seq((LONG, LONG), (LONG, LONG), (TYPEREF("User"), TYPEREF("User")))) shouldBe Right(Map.empty)
    }
    "fails if no simple common type" in {
      TypeInferrer(Seq((LONG, BYTEVECTOR))) should produce("Non-matching types")
    }

    "fails if no obj common type" in {
      TypeInferrer(Seq((TYPEREF("User"), TYPEREF("Admin")))) should produce("Non-matching types")
    }
  }

  "inferring" - {
    "simple type" in {
      TypeInferrer(Seq((LONG, typeparamT))) shouldBe Right(Map(typeparamT -> LONG))
    }

    "many simple types" in {
      TypeInferrer(
        Seq(
          (LONG, typeparamT),
          (BYTEVECTOR, typeparamG)
        )) shouldBe Right(Map(typeparamT -> LONG, typeparamG -> BYTEVECTOR))
    }

    "one simple same type" in {
      TypeInferrer(Seq((LONG, typeparamT), (LONG, typeparamT), (LONG, typeparamT))) shouldBe Right(Map(typeparamT -> LONG))
    }

    "option" - {
      "as plain type" in {
        TypeInferrer(Seq((OPTION(LONG), typeparamT))) shouldBe Right(Map(typeparamT -> OPTION(LONG)))
      }

      "containing inner type" in {
        TypeInferrer(Seq((OPTION(LONG), OPTIONTYPEPARAM(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "containing inner and separate type" in {
        TypeInferrer(Seq((LONG, typeparamT), (OPTION(LONG), OPTIONTYPEPARAM(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "containing best common type" in {
        TypeInferrer(Seq((LONG, typeparamT), (OPTION(NOTHING), OPTIONTYPEPARAM(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "fails if no common type" in {
        TypeInferrer(Seq((BYTEVECTOR, typeparamT), (BYTEVECTOR, OPTIONTYPEPARAM(typeparamT)))) should produce("Non-matching types")
        TypeInferrer(Seq((LONG, typeparamT), (OPTION(OPTION(NOTHING)), OPTIONTYPEPARAM(typeparamT)))) should produce("Can't match inferred types")
        TypeInferrer(Seq((BYTEVECTOR, typeparamT), (OPTION(LONG), OPTIONTYPEPARAM(typeparamT)))) should produce("Can't match inferred types")
        TypeInferrer(Seq((OPTION(BYTEVECTOR), typeparamT), (OPTION(LONG), OPTIONTYPEPARAM(typeparamT)))) should produce("Can't match inferred types")
      }
    }
  }
}
