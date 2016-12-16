package scorex.transaction.assets.exchange

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.TransactionGen
import scorex.utils.ByteArray
import OrderJson._

class OrderCancelTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("OrderCancelTransaction transaction serialization roundtrip") {
    forAll(orderCancelGenerator) { x: (OrderCancelTransaction, PrivateKeyAccount) =>
      val (oc, pk) = x
      val r = OrderCancelTransaction.parseBytes(oc.bytes).get
      r.bytes shouldEqual oc.bytes
      r.id shouldBe oc.id
      r.sender shouldBe oc.sender
      ByteArray.sameOption(r.spendAssetId, oc.spendAssetId) shouldBe true
      ByteArray.sameOption(r.receiveAssetId, oc.receiveAssetId) shouldBe true
      r.fee shouldBe oc.fee
      r.timestamp shouldBe oc.timestamp
      r.signature shouldBe oc.signature
    }
  }

  property("OrderCancelTransaction generator should generate valid orders") {
    forAll(orderCancelGenerator) { x: (OrderCancelTransaction, PrivateKeyAccount) =>
      val (oc, pk) = x
      oc.isValid shouldBe valid
    }
  }


  property("Order signature validation") {
    forAll(orderCancelGenerator, bytes32gen) {(x: (OrderCancelTransaction, PrivateKeyAccount), bytes: Array[Byte]) =>
      val (oc, pk) = x
      oc.isValid shouldBe valid
      oc.copy(sender = new PublicKeyAccount(bytes)).isValid should contain ("signature should be valid")
      oc.copy(spendAssetId = oc.spendAssetId.map(Array(0: Byte) ++ _).orElse(Some(Array(0: Byte)))).
        isValid should contain ("signature should be valid")
      oc.copy(receiveAssetId = oc.receiveAssetId.map(Array(0: Byte) ++ _).orElse(Some(Array(0: Byte)))).
        isValid should contain ("signature should be valid")
      oc.copy(fee = oc.fee + 1).isValid should contain ("signature should be valid")
      oc.copy(timestamp = oc.timestamp + 1).isValid should contain ("signature should be valid")
      oc.copy(signature = bytes ++ bytes).isValid should contain ("signature should be valid")
    }
  }

  property("Order json roundtrip ") {
    forAll(orderCancelGenerator) {(x: (OrderCancelTransaction, PrivateKeyAccount)) =>
      val (oc, pk) = x
      oc.isValid shouldBe valid
      val c = oc.json.validate[OrderCancelTransaction].get
      c.isValid shouldBe valid
    }
  }
}
