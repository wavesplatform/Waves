package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.Types._
import org.scalatest.{FreeSpec, Matchers}
import Common._
import com.wavesplatform.lang.v1.compiler.TypeInferrer

class TypeInferrerTest extends FreeSpec with Matchers {

  val typeparamT = TYPEPARAM('T')
  val typeparamG = TYPEPARAM('G')

  "no types to infer" - {
    "all types are correct" in {
      TypeInferrer(Seq((LONG, LONG), (LONG, LONG), (CASETYPEREF("User", List()), CASETYPEREF("User", List())))) shouldBe Right(Map.empty)
    }
    "fails if no simple common type" in {
      TypeInferrer(Seq((LONG, BYTEVECTOR))) should produce("Non-matching types")
    }

    "fails if no obj common type" in {
      TypeInferrer(Seq((CASETYPEREF("User", List()), CASETYPEREF("Admin", List())))) should produce("Non-matching types")
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
        TypeInferrer(Seq((LIST(LONG), typeparamT))) shouldBe Right(Map(typeparamT -> LIST(LONG)))
      }

      "containing inner type" in {
        TypeInferrer(Seq((LIST(LONG), LISTTYPEPARAM(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "containing inner and separate type" in {
        TypeInferrer(Seq((LONG, typeparamT), (LIST(LONG), LISTTYPEPARAM(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "containing best common type" in {
        TypeInferrer(Seq((LONG, typeparamT), (LIST(NOTHING), LISTTYPEPARAM(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "fails if no common type" in {
        TypeInferrer(Seq((BYTEVECTOR, typeparamT), (BYTEVECTOR, LISTTYPEPARAM(typeparamT)))) should produce("Non-matching types")
        TypeInferrer(Seq((LONG, typeparamT), (LIST(LIST(NOTHING)), LISTTYPEPARAM(typeparamT)))) should produce("Can't match inferred types")
        TypeInferrer(Seq((BYTEVECTOR, typeparamT), (LIST(LONG), LISTTYPEPARAM(typeparamT)))) should produce("Can't match inferred types")
//        TypeInferrer(Seq((OPTION(BYTEVECTOR), typeparamT), (LIST(LONG), LISTTYPEPARAM(typeparamT)))) should produce("Can't match inferred types")
      }
    }
  }
}
