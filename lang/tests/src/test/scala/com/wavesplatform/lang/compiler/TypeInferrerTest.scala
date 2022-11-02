package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.v1.compiler.TypeInferrer
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.test.*

class TypeInferrerTest extends FreeSpec {

  val typeparamT = TYPEPARAM('T')
  val typeparamG = TYPEPARAM('G')

  "no types to infer" - {
    "all types are correct" in {
      TypeInferrer(Seq((STRING, STRING), (STRING, STRING), (CASETYPEREF("User", List()), CASETYPEREF("User", List()))),
                   Map("User" -> CASETYPEREF("User", List.empty))) shouldBe Right(Map.empty)
    }
    "fails if no simple common type" in {
      TypeInferrer(Seq((LONG, BYTESTR))) should produce("Non-matching types")
    }

    "fails if no obj common type" in {
      TypeInferrer(Seq((CASETYPEREF("User", List()), CASETYPEREF("Admin", List()))),
                   Map("User" -> CASETYPEREF("User", List.empty), "Admin" -> CASETYPEREF("Admin", List.empty))) should produce("Non-matching types")
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
          (BYTESTR, typeparamG)
        )) shouldBe Right(Map(typeparamT -> LONG, typeparamG -> BYTESTR))
    }

    "one simple same type" in {
      TypeInferrer(Seq((LONG, typeparamT), (LONG, typeparamT), (LONG, typeparamT))) shouldBe Right(Map(typeparamT -> LONG))
    }

    "option" - {
      "as plain type" in {
        TypeInferrer(Seq((LIST(LONG), typeparamT))) shouldBe Right(Map(typeparamT -> LIST(LONG)))
      }

      "containing inner type" in {
        TypeInferrer(Seq((LIST(LONG), PARAMETERIZEDLIST(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "containing same inner type" in {
        TypeInferrer(Seq((LIST(LONG), PARAMETERIZEDLIST(typeparamT)), (LIST(LONG), PARAMETERIZEDLIST(typeparamT)))) shouldBe Right(
          Map(typeparamT -> LONG))
      }

      "containing inner and separate type" in {
        TypeInferrer(Seq((LONG, typeparamT), (LIST(LONG), PARAMETERIZEDLIST(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "containing best common type" in {
        TypeInferrer(Seq((LONG, typeparamT), (LIST(NOTHING), PARAMETERIZEDLIST(typeparamT)))) shouldBe Right(Map(typeparamT -> LONG))
      }

      "fails if no common type" in {
        TypeInferrer(Seq((BYTESTR, typeparamT), (BYTESTR, PARAMETERIZEDLIST(typeparamT)))) should produce("Non-matching types")
        TypeInferrer(Seq((LONG, typeparamT), (LIST(LIST(NOTHING)), PARAMETERIZEDLIST(typeparamT)))) should produce("Can't match inferred types")
        TypeInferrer(Seq((BYTESTR, typeparamT), (LIST(LONG), PARAMETERIZEDLIST(typeparamT)))) should produce("Can't match inferred types")
      }
    }

    "union" - {
      val optionLong = UNION(LONG, UNIT)

      "no types to infer" - {
        "simple types" in {
          TypeInferrer(Seq((LONG, optionLong))) shouldBe Right(Map.empty)
        }

        "no common simple type" in {
          TypeInferrer(Seq((LONG, UNION(BOOLEAN, UNIT)))) should produce("Non-matching types")
        }

        "inside list" in {
          TypeInferrer(Seq((LIST(LONG), LIST(optionLong)))) shouldBe Right(Map.empty)
        }

        "no common type inside list" in {
          TypeInferrer(Seq((LIST(LONG), LIST(UNION(BOOLEAN, UNIT))))) should produce("Non-matching types")
        }

        "unit in union" in {
          TypeInferrer(Seq((UNIT, UNION(BOOLEAN, UNIT)))) shouldBe Right(Map.empty)
        }
      }

      "inferring" - {
        val optionT = PARAMETERIZEDUNION(List(typeparamT, UNIT))

        "simple types" in {
          TypeInferrer(Seq((LONG, optionT))) shouldBe Right(Map(typeparamT -> LONG))
        }

        "inside list" in {
          TypeInferrer(Seq((LIST(LONG), PARAMETERIZEDLIST(optionT)))) shouldBe Right(Map(typeparamT -> LONG))
        }

        "inside union" in {
          TypeInferrer(Seq((optionLong, optionT))) shouldBe Right(Map(typeparamT -> LONG))
        }

        "Option[Int] matches type parameter" in {
          TypeInferrer(Seq((optionLong, typeparamT))) shouldBe Right(Map(typeparamT -> optionLong))
        }

        "common type of Int and Option[Int] should be Option[Int]" in {
          TypeInferrer(Seq((optionLong, typeparamT), (LONG, typeparamT))) shouldBe Right(Map(typeparamT -> optionLong))
        }

        "ambiguous inference" in {
          TypeInferrer(Seq((LONG, PARAMETERIZEDUNION(List(typeparamT, typeparamG))))) should produce("Can't resolve correct type")
        }
      }

      "Lists" in {
        TypeInferrer(Seq( /*(LONG, typeparamT),*/ (LIST(NOTHING), PARAMETERIZEDLIST(typeparamG)))) shouldBe Right(
          Map( /*typeparamT -> LONG,*/ typeparamG -> NOTHING))
      }
    }
  }
}
