package com.wavesplatform.transaction.smart
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.{V1, V2, V3}
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CaseObj}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.traits.domain.{APair, Ord, OrdType, Recipient}
import org.scalatest.{FreeSpec, Matchers}

class OrderProofTest extends FreeSpec with Matchers {

  private def getBindingObjProofStr(obj: CaseObj): Option[String] =
    obj.fields.get("proofs").map(arr => arr.asInstanceOf[ARR].xs.toList.head.toString)

  "OrderBindings" - {
    "order obj for account scripts and V1/V2 should contains proofs" in {
      val ord = Ord(
        id = ByteStr.empty,
        sender = Recipient.Address(ByteStr.empty),
        senderPublicKey = ByteStr.empty,
        matcherPublicKey = ByteStr.empty,
        assetPair = APair(None, None),
        orderType = OrdType.Buy,
        price = 0L,
        amount = 0L,
        timestamp = 0L,
        expiration = 0L,
        matcherFee = 0L,
        bodyBytes = ByteStr.empty,
        proofs = List(ByteStr.fromLong(42L)).toIndexedSeq
      )

      getBindingObjProofStr(Bindings.orderObject(ord, true, V1)) shouldBe Some("1111111j")
      getBindingObjProofStr(Bindings.orderObject(ord, true, V2)) shouldBe Some("1111111j")
      getBindingObjProofStr(Bindings.orderObject(ord, true, V3)) shouldBe Some("1111111j")
      getBindingObjProofStr(Bindings.orderObject(ord, false, V1)) shouldBe Some("1111111j")
      getBindingObjProofStr(Bindings.orderObject(ord, false, V2)) shouldBe Some("1111111j")
    }

    "order obj for asset scripts and V3 should not contains proofs" in {
      val ord = Ord(
        id = ByteStr.empty,
        sender = Recipient.Address(ByteStr.empty),
        senderPublicKey = ByteStr.empty,
        matcherPublicKey = ByteStr.empty,
        assetPair = APair(None, None),
        orderType = OrdType.Buy,
        price = 0L,
        amount = 0L,
        timestamp = 0L,
        expiration = 0L,
        matcherFee = 0L,
        bodyBytes = ByteStr.empty,
        proofs = List(ByteStr.fromLong(42L)).toIndexedSeq
      )

      getBindingObjProofStr(Bindings.orderObject(ord, false, V3)) shouldBe None
    }
  }
}
