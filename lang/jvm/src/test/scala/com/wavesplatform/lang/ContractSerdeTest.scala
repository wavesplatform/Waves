package com.wavesplatform.lang

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.contract.{ContractSerDe, DApp}
import com.wavesplatform.lang.v1.compiler.Terms._
import org.scalatest.{Assertion, FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractSerdeTest extends FreeSpec with PropertyChecks with Matchers with NoShrink {

  def roundTrip(c: DApp): Assertion = {
    val bytes = ContractSerDe.serialize(c)
    val conEi = ContractSerDe.deserialize(bytes)

    conEi shouldBe 'right
    conEi.right.get shouldBe c
  }

  "roundtrip" - {

    "empty" in roundTrip(DApp(ByteStr.empty, Nil, Nil, None))

//    "empty" in {
//      val cf = ContractFunction(
//        CallableAnnotation("whoooo"),
//        FUNC("anotherFunc", List("argssss"), CONST_BOOLEAN(true))
//      )
//      val bytes = ContractSerDe.serializeContractFunction(,cf)
//
//    }

    "one-declaration" in roundTrip(
      DApp(
        ByteStr.empty,
        List(
          LET("letName", CONST_BOOLEAN(true))
        ),
        List.empty,
        None
      ))

    "two-declarations" in roundTrip(
      DApp(
        ByteStr.empty,
        List(
          LET("letName", CONST_BOOLEAN(true)),
          FUNC("funcName", List("arg1", "arg2"), CONST_BOOLEAN(false))
        ),
        List.empty,
        None
      ))

    "callable function" in roundTrip(
      DApp(
        ByteStr.empty,
        List(),
        List(
          CallableFunction(
            CallableAnnotation("sender"),
            FUNC("foo", List("a"), REF("a"))
          )
        ),
        None
      ))

    "default function" in roundTrip(
      DApp(
        ByteStr.empty,
        List(),
        List(
          CallableFunction(
            CallableAnnotation("sender"),
            FUNC("default", List(), TRUE)
          )
        ),
        None,
      )
    )

    "verifier function" in roundTrip(
      DApp(
        ByteStr.empty,
        List(),
        List(),
        Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
      )
    )

    "full contract" in roundTrip(
      DApp(
        ByteStr.empty,
        List(
          LET("letName", CONST_BOOLEAN(true)),
          FUNC("funcName", List("arg1", "arg2"), CONST_BOOLEAN(false))
        ),
        List(
          CallableFunction(
            CallableAnnotation("whoooo"),
            FUNC("anotherFunc", List("argssss"), CONST_BOOLEAN(true))
          ),
          CallableFunction(
            CallableAnnotation("whoooo"),
            FUNC("default", List(), CONST_BOOLEAN(false))
          )
        ),
        Some(
          VerifierFunction(
            VerifierAnnotation("hmmm"),
            FUNC("funcAgain", List("arg"), CONST_BOOLEAN(false))
          )
        )
      ))
  }
}
