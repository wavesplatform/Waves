package com.wavesplatform.transaction

import com.wavesplatform.OrderOps._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.ValidationMatcher
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, _}
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.{NTPTime, TransactionGen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class OrderSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen with ValidationMatcher with NTPTime {

  property("Order transaction serialization roundtrip") {
    forAll(orderV1Gen) { order =>
      val recovered = OrderV1.parseBytes(order.bytes()).get
      recovered.bytes() shouldEqual order.bytes()
      recovered.idStr() shouldBe order.idStr()
      recovered.senderPublicKey.publicKey shouldBe order.senderPublicKey.publicKey
      recovered.matcherPublicKey shouldBe order.matcherPublicKey
      recovered.assetPair shouldBe order.assetPair
      recovered.orderType shouldBe order.orderType
      recovered.price shouldBe order.price
      recovered.amount shouldBe order.amount
      recovered.timestamp shouldBe order.timestamp
      recovered.expiration shouldBe order.expiration
      recovered.matcherFee shouldBe order.matcherFee
      recovered.signature shouldBe order.signature
    }
    forAll(orderV2Gen) { order =>
      val recovered = OrderV2.parseBytes(order.bytes()).get
      recovered.bytes() shouldEqual order.bytes()
      recovered.idStr() shouldBe order.idStr()
      recovered.senderPublicKey.publicKey shouldBe order.senderPublicKey.publicKey
      recovered.matcherPublicKey shouldBe order.matcherPublicKey
      recovered.assetPair shouldBe order.assetPair
      recovered.orderType shouldBe order.orderType
      recovered.price shouldBe order.price
      recovered.amount shouldBe order.amount
      recovered.timestamp shouldBe order.timestamp
      recovered.expiration shouldBe order.expiration
      recovered.matcherFee shouldBe order.matcherFee
      recovered.signature shouldBe order.signature
    }
  }

  property("Order generator should generate valid orders") {
    forAll(orderGen) { order =>
      order.isValid(ntpTime.correctedTime()) shouldBe valid
    }
  }

  property("Order timestamp validation") {
    forAll(orderGen) { order =>
      val time = ntpTime.correctedTime()
      order.updateTimestamp(-1).isValid(time) shouldBe not(valid)
    }
  }

  property("Order expiration validation") {
    forAll(arbitraryOrderGen) { order =>
      val isValid = order.isValid(ntpTime.correctedTime())
      val time    = ntpTime.correctedTime()
      whenever(order.expiration < time || order.expiration > time + Order.MaxLiveTime) {
        isValid shouldBe not(valid)
      }
    }
  }

  property("Order amount validation") {
    forAll(arbitraryOrderGen) { order =>
      whenever(order.amount <= 0) {
        order.isValid(ntpTime.correctedTime()) shouldBe not(valid)
      }
    }
  }

  property("Order matcherFee validation") {
    forAll(arbitraryOrderGen) { order =>
      whenever(order.matcherFee <= 0) {
        order.isValid(ntpTime.correctedTime()) shouldBe not(valid)
      }
    }
  }

  property("Order price validation") {
    forAll(arbitraryOrderGen) { order =>
      whenever(order.price <= 0) {
        order.isValid(ntpTime.correctedTime()) shouldBe not(valid)
      }
    }
  }

  property("Order signature validation") {
    val err = "proof doesn't validate as signature"
    forAll(orderGen, accountGen) {
      case (order, pka) =>
        Verifier.verifyAsEllipticCurveSignature(order) shouldBe an[Right[_, _]]
        Verifier.verifyAsEllipticCurveSignature(order.updateSender(pka)) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.updateMatcher(pka)) should produce(err)
        val assetPair = order.assetPair
        Verifier.verifyAsEllipticCurveSignature(
          order
            .updatePair(assetPair.copy(
              amountAsset = assetPair.amountAsset.map(Array(0: Byte) ++ _.arr).orElse(Some(Array(0: Byte))).map(ByteStr(_))))) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order
          .updatePair(assetPair.copy(priceAsset = assetPair.priceAsset.map(Array(0: Byte) ++ _.arr).orElse(Some(Array(0: Byte))).map(ByteStr(_))))) should produce(
          err)
        Verifier.verifyAsEllipticCurveSignature(order.updateType(OrderType.reverse(order.orderType))) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.updatePrice(order.price + 1)) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.updateAmount(order.amount + 1)) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.updateExpiration(order.expiration + 1)) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.updateFee(order.matcherFee + 1)) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.updateProofs(Proofs(Seq(ByteStr(pka.publicKey ++ pka.publicKey))))) should produce(err)
    }
  }

  property("Buy and Sell orders") {
    forAll(orderParamGen) {
      case (sender, matcher, pair, _, amount, price, timestamp, _, _) =>
        val expiration = timestamp + Order.MaxLiveTime - 1000
        val buy        = Order.buy(sender, matcher, pair, amount, price, timestamp, expiration, price)
        buy.orderType shouldBe OrderType.BUY

        val sell = Order.sell(sender, matcher, pair, amount, price, timestamp, expiration, price)
        sell.orderType shouldBe OrderType.SELL
    }
  }

  property("AssetPair test") {
    forAll(assetIdGen, assetIdGen) { (assetA: Option[AssetId], assetB: Option[AssetId]) =>
      whenever(assetA != assetB) {
        val pair = AssetPair(assetA, assetB)
        pair.isValid shouldBe valid
      }
    }
  }

}
