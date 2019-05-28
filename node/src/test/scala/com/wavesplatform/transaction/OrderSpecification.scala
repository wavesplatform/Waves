package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, _}
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.{NTPTime, TransactionGen}
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Random

class OrderSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen with ValidationMatcher with NTPTime {

  private def checkFieldsEquality(left: Order, right: Order): Assertion = {

    def defaultChecks: Assertion = {
      left.bytes() shouldEqual right.bytes()
      left.idStr() shouldBe right.idStr()
      left.senderPublicKey shouldBe right.senderPublicKey
      left.matcherPublicKey shouldBe right.matcherPublicKey
      left.assetPair shouldBe right.assetPair
      left.orderType shouldBe right.orderType
      left.price shouldBe right.price
      left.amount shouldBe right.amount
      left.timestamp shouldBe right.timestamp
      left.expiration shouldBe right.expiration
      left.matcherFee shouldBe right.matcherFee
      left.signature shouldBe right.signature
    }

    (left, right) match {
      case (l: OrderV3, r: OrderV3) => defaultChecks; l.matcherFeeAssetId shouldBe r.matcherFeeAssetId
      case _                        => defaultChecks
    }
  }

  property("Order transaction serialization roundtrip") {

    forAll(orderV1Gen) { order =>
      val recovered = OrderV1.parseBytes(order.bytes()).get
      checkFieldsEquality(recovered, order)
    }

    forAll(orderV2Gen) { order =>
      val recovered = OrderV2.parseBytes(order.bytes()).get
      checkFieldsEquality(recovered, order)
    }

    forAll(orderV3Gen) { order =>
      val recovered = OrderV3.parseBytes(order.bytes()).get
      checkFieldsEquality(recovered, order)
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
    val err = "Proof doesn't validate as signature"
    forAll(orderGen, accountGen) {
      case (order, pka) =>
        val rndAsset = Array[Byte](32)

        Random.nextBytes(rndAsset)

        Verifier.verifyAsEllipticCurveSignature(order) shouldBe an[Right[_, _]]
        Verifier.verifyAsEllipticCurveSignature(order.updateSender(pka)) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.updateMatcher(pka)) should produce(err)
        val assetPair = order.assetPair
        Verifier.verifyAsEllipticCurveSignature(
          order
            .updatePair(assetPair.copy(amountAsset = IssuedAsset(ByteStr(rndAsset))))) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(
          order
            .updatePair(assetPair.copy(priceAsset = IssuedAsset(ByteStr(rndAsset))))) should produce(err)
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
    forAll(assetIdGen, assetIdGen) { (assetA, assetB) =>
      whenever(assetA != assetB) {
        val assetAId: Asset = assetA.fold[Asset](Waves)(arr => IssuedAsset(arr))
        val assetBId: Asset = assetB.fold[Asset](Waves)(arr => IssuedAsset(arr))

        val pair = AssetPair(assetAId, assetBId)

        pair.isValid shouldBe valid
      }
    }
  }

}
