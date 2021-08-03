package com.wavesplatform.lang

import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.contract.{ContractSerDe, DApp}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.test._
import org.scalatest.Assertion

class ContractSerdeTest extends FreeSpec {

  def roundTrip(c: DApp): Assertion = {
    val bytes = ContractSerDe.serialize(c)
    val conEi = bytes.flatMap(ContractSerDe.deserialize)

    conEi shouldBe Right(c)
  }

  "roundtrip" - {

    "empty" in roundTrip(DApp(DAppMeta(), Nil, Nil, None))

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
        DAppMeta(),
        List(
          LET("letName", CONST_BOOLEAN(true))
        ),
        List.empty,
        None
      )
    )

    "two-declarations" in roundTrip(
      DApp(
        DAppMeta(),
        List(
          LET("letName", CONST_BOOLEAN(true)),
          FUNC("funcName", List("arg1", "arg2"), CONST_BOOLEAN(false))
        ),
        List.empty,
        None
      )
    )

    "callable function" in roundTrip(
      DApp(
        DAppMeta(),
        List(),
        List(
          CallableFunction(
            CallableAnnotation("sender"),
            FUNC("foo", List("a"), REF("a"))
          )
        ),
        None
      )
    )

    "default function" in roundTrip(
      DApp(
        DAppMeta(),
        List(),
        List(
          CallableFunction(
            CallableAnnotation("sender"),
            FUNC("default", List(), TRUE)
          )
        ),
        None
      )
    )

    "verifier function" in roundTrip(
      DApp(
        DAppMeta(),
        List(),
        List(),
        Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
      )
    )

    "full contract" in roundTrip(
      DApp(
        DAppMeta(),
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
      )
    )

    "full contract with meta" in roundTrip(
      DApp(
        DAppMeta(
          version = 1,
          List(
            CallableFuncSignature(ByteString.copyFrom(Array[Byte](0, 1, 2, 3))),
            CallableFuncSignature(ByteString.copyFrom(Array[Byte](3, 2, 1, 0))),
            CallableFuncSignature(ByteString.EMPTY)
          )
        ),
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
      )
    )
  }

  "limitations" - {
    def oneCallableDApp(name: String) =
      DApp(
        DAppMeta(),
        Nil,
        List(
          CallableFunction(
            CallableAnnotation("i"),
            FUNC(name, Nil, ARR(IndexedSeq(), limited = false).explicitGet())
          )
        ),
        None
      )

    "callable name limit" in {
      val limit = ContractLimits.MaxDeclarationNameInBytes

      roundTrip(oneCallableDApp("a" * limit))

      ContractSerDe.serialize(oneCallableDApp("a" * (limit + 1)))
        .flatMap(ContractSerDe.deserialize) should produce(
           s"Callable function name (${"a" * (limit + 1)}) size = ${limit + 1} bytes " +
           s"exceeds $limit"
        )
    }
  }
}
