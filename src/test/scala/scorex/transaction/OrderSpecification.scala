package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.utils.{ByteArray, NTP}

class OrderSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Order transaction serialization roundtrip") {
    forAll { x: (Order, PrivateKeyAccount) =>
      val (order, pk) = x
      val recovered = Order.parseBytes(order.bytes).get
      recovered.bytes shouldEqual order.bytes
      recovered.id shouldBe order.id
      recovered.sender shouldBe order.sender
      recovered.matcher shouldBe order.matcher
      ByteArray.sameOption(recovered.spendAssetId, order.spendAssetId) shouldBe true
      ByteArray.sameOption(recovered.receiveAssetId, order.receiveAssetId) shouldBe true
      recovered.price shouldBe order.price
      recovered.amount shouldBe order.amount
      recovered.timestamp shouldBe order.timestamp
      recovered.expiration shouldBe order.expiration
      recovered.matcherFee shouldBe order.matcherFee
      recovered.signature shouldBe order.signature
    }
  }

  property("Order generator should generate valid orders") {
    forAll { x: (Order, PrivateKeyAccount) =>
      val (order, pk) = x
      order.isValid(NTP.correctedTime()) shouldBe valid
    }
  }

  property("Order timestamp validation") {
    forAll { x: (Order, PrivateKeyAccount) =>
      val (order, pk) = x
      val time = NTP.correctedTime()
      order.copy(timestamp = -1).isValid(time) shouldBe not(valid)
      order.copy(timestamp = time + 1000).isValid(time) should contain ("timestamp should be before created before execution")
    }
  }

  property("Order expiration validation") {
    forAll(invalidOrderGenerator) { order: Order =>
      val isValid = order.isValid(NTP.correctedTime())
      val time = NTP.correctedTime()
      whenever(order.expiration < time || order.expiration > time + Order.MaxLiveTime) {
        isValid shouldBe not(valid)
      }
    }
  }

  property("Order amount validation") {
    forAll(invalidOrderGenerator) { order: Order =>
      whenever(order.amount <= 0) {
        order.isValid(NTP.correctedTime()) shouldBe not(valid)
      }
    }
  }

  property("Order matcherFee validation") {
    forAll(invalidOrderGenerator) { order: Order =>
      whenever(order.matcherFee <= 0) {
        order.isValid(NTP.correctedTime()) shouldBe not(valid)
      }
    }
  }

  property("Order price validation") {
    forAll(invalidOrderGenerator) { order: Order =>
      whenever(order.price <= 0) {
        order.isValid(NTP.correctedTime()) shouldBe not(valid)
      }
    }
  }

  property("Order signature validation") {
    forAll { (x: (Order, PrivateKeyAccount), bytes: Array[Byte]) =>
      val (order, pk) = x
      order.isValid(NTP.correctedTime()) shouldBe valid
      order.copy(sender = new PublicKeyAccount(bytes)).isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(matcher = new PublicKeyAccount(bytes)).isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(spendAssetId = order.spendAssetId.map(Array(0: Byte) ++ _).orElse(Some(Array(0: Byte)))).
        isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(receiveAssetId = order.receiveAssetId.map(Array(0: Byte) ++ _).orElse(Some(Array(0: Byte)))).
        isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(price = order.price + 1).isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(amount = order.amount + 1).isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(expiration = order.expiration + 1).isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(matcherFee = order.matcherFee + 1).isValid(NTP.correctedTime()) should contain ("signature should be valid")
      order.copy(signature = bytes ++ bytes).isValid(NTP.correctedTime()) should contain ("signature should be valid")
    }
  }

  property("Buy and Sell orders") {
    forAll(accountGen, accountGen, assetPairGen, positiveLongGen, positiveLongGen, timestampGen) {
      (sender: PrivateKeyAccount, matcher: PrivateKeyAccount, pair: AssetPair,
       price: Long, amount: Long, timestamp: Long) =>
        val expiration = timestamp + Order.MaxLiveTime - 1000
        val buy = Order.buy(sender, matcher, pair, price, amount, timestamp, expiration, price)
        buy.orderType shouldBe OrderType.BUY

        val sell = Order.sell(sender, matcher, pair, price, amount, timestamp, expiration, price)
        sell.orderType shouldBe OrderType.SELL
    }
  }

  property("AssetPair test") {
    forAll(assetIdGen, assetIdGen) { (assetA: Option[AssetId], assetB: Option[AssetId]) =>
      whenever(!ByteArray.sameOption(assetA, assetB)) {
        val pair = AssetPair(assetA, assetB)
        ByteArray.compare(pair.first, pair.second) should be < 0
      }
    }
  }


}
