package com.wavesplatform.lang

import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.serialization.{ContractSerDe, ContractSerDeV1, ContractSerDeV2}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.test._
import org.scalatest.Assertion

class ContractSerdeTest extends FreeSpec {

  def roundTrip(c: DApp, contractSerDe: ContractSerDe): Assertion = {
    val bytes = contractSerDe.serialize(c)
    val conEi = bytes.flatMap(contractSerDe.deserialize)

    conEi shouldBe Right(c)
  }

  val serializers = List(ContractSerDeV1, ContractSerDeV2)

  "roundtrip" - {

    "empty" in serializers.foreach(roundTrip(DApp(DAppMeta(), Nil, Nil, None), _))


//    "empty" in {
//      val cf = ContractFunction(
//        CallableAnnotation("whoooo"),
//        FUNC("anotherFunc", List("argssss"), CONST_BOOLEAN(true))
//      )
//      val bytes = ContractSerDe.serializeContractFunction(,cf)
//
//    }

    "one-declaration" in serializers.foreach { ser =>
      roundTrip(
        DApp(
          DAppMeta(),
          List(
            LET("letName", CONST_BOOLEAN(true))
          ),
          List.empty,
          None
        ),
        ser
      )
    }



    "two-declarations" in serializers.foreach { ser =>
      roundTrip(
        DApp(
          DAppMeta(),
          List(
            LET("letName", CONST_BOOLEAN(true)),
            FUNC("funcName", List("arg1", "arg2"), CONST_BOOLEAN(false))
          ),
          List.empty,
          None
        ),
        ser
      )
    }

    "callable function" in serializers.foreach { ser =>
      roundTrip(
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
        ),
        ser
      )
    }

    "default function" in serializers.foreach { ser =>
      roundTrip(
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
        ),
        ser
      )
    }

    "verifier function" in serializers.foreach { ser =>
      roundTrip(
        DApp(
          DAppMeta(),
          List(),
          List(),
          Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
        ),
        ser
      )
    }

    "full contract" in serializers.foreach { ser =>
      roundTrip(
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
        ),
        ser
      )
    }

    "full contract with meta" in serializers.foreach { ser =>
      roundTrip(
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
        ),
        ser
      )
    }
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

    "callable name limit" in serializers.foreach { ser =>
      val limit = ContractLimits.MaxDeclarationNameInBytes

      roundTrip(oneCallableDApp("a" * limit), ser)

      ser.serialize(oneCallableDApp("a" * (limit + 1)))
        .flatMap(ser.deserialize) should produce(
           s"Callable function name (${"a" * (limit + 1)}) size = ${limit + 1} bytes " +
           s"exceeds $limit"
        )
    }
  }
}
