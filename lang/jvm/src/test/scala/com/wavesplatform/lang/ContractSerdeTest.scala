package com.wavesplatform.lang

import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.contract.{Contract, ContractSerDe}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, FUNC, LET, REF}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FreeSpec, Matchers}

class ContractSerdeTest extends FreeSpec with PropertyChecks with Matchers with NoShrink {

  def roundTrip(c: Contract): Assertion = {
    val bytes = ContractSerDe.serialize(c)
    val conEi = ContractSerDe.deserialize(bytes)

    conEi shouldBe 'right
    conEi.right.get shouldBe c
  }

  "roundtrip" - {

    "empty" in roundTrip(Contract(Nil, Nil, None))

//    "empty" in {
//      val cf = ContractFunction(
//        CallableAnnotation("whoooo"),
//        FUNC("anotherFunc", List("argssss"), CONST_BOOLEAN(true))
//      )
//      val bytes = ContractSerDe.serializeContractFunction(,cf)
//
//    }

    "non-empty" in roundTrip(
      Contract(
        List(
          LET("letName", CONST_BOOLEAN(true)),
          FUNC("funcName", List("arg1", "arg2"), CONST_BOOLEAN(false))
        ),
        List(
          ContractFunction(
            CallableAnnotation("whoooo"),
            FUNC("anotherFunc", List("argssss"), CONST_BOOLEAN(true))
          )
        ),
        Some(
          VerifierFunction(
            VerifierAnnotation("hmmm"),
            FUNC("funcAgain", List("arg"), CONST_BOOLEAN(false))
          )
        )
      ))

    "simple" in roundTrip(
      Contract(
        List(),
        List(
          ContractFunction(
            CallableAnnotation("sender"),
            FUNC("foo", List("a"), REF("a"))
          )
        ),
        None
      ))
  }

}
