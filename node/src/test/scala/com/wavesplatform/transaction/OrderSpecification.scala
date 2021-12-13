package com.wavesplatform.transaction

import com.wavesplatform.NTPTime
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.smart.Verifier
import org.scalatest._

import scala.util.Random

class OrderSpecification extends PropSpec with ValidationMatcher with NTPTime {
  property("Order serialization roundtrip") {
    forAll(orderV1Gen) { order =>
      val recovered = Order.parseBytes(Order.V1, order.bytes()).get
      checkFieldsEquality(recovered, order)
    }

    forAll(orderV2Gen) { order =>
      val recovered = Order.parseBytes(Order.V2, order.bytes()).get
      checkFieldsEquality(recovered, order)
    }

    forAll(orderV3Gen) { order =>
      val recovered = Order.parseBytes(Order.V3, order.bytes()).get
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
      order.copy(timestamp = -1).isValid(time) shouldBe not(valid)
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

        Verifier.verifyAsEllipticCurveSignature(order, checkWeakPk = false) shouldBe an[Right[_, _]]
        Verifier.verifyAsEllipticCurveSignature(order.copy(senderPublicKey = pka.publicKey), checkWeakPk = false) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.copy(matcherPublicKey = pka.publicKey), checkWeakPk = false) should produce(err)
        val assetPair = order.assetPair
        Verifier.verifyAsEllipticCurveSignature(
          order
            .copy(assetPair = assetPair.copy(amountAsset = IssuedAsset(ByteStr(rndAsset)))),
          checkWeakPk = false
        ) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(
          order
            .copy(assetPair = assetPair.copy(priceAsset = IssuedAsset(ByteStr(rndAsset)))),
          checkWeakPk = false
        ) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.copy(orderType = OrderType.reverse(order.orderType)), checkWeakPk = false) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.copy(price = order.price + 1), checkWeakPk = false) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.copy(amount = order.amount + 1), checkWeakPk = false) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.copy(expiration = order.expiration + 1), checkWeakPk = false) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.copy(matcherFee = order.matcherFee + 1), checkWeakPk = false) should produce(err)
        Verifier.verifyAsEllipticCurveSignature(order.copy(proofs = Proofs(Seq(ByteStr(pka.publicKey.arr ++ pka.publicKey.arr)))), checkWeakPk = false) should produce(err)
    }
  }

  property("Buy and Sell orders") {
    forAll(orderParamGen) {
      case (sender, matcher, pair, _, amount, price, timestamp, _, _) =>
        val expiration = timestamp + Order.MaxLiveTime - 1000
        val buy = Order.buy(
          Order.V1,
          sender = sender,
          matcher = matcher.publicKey,
          pair = pair,
          amount = amount,
          price = price,
          timestamp = timestamp,
          expiration = expiration,
          matcherFee = price
        )
        buy.orderType shouldBe OrderType.BUY

        val sell = Order.sell(
          Order.V1,
          sender = sender,
          matcher = matcher.publicKey,
          pair = pair,
          amount = amount,
          price = price,
          timestamp = timestamp,
          expiration = expiration,
          matcherFee = price
        )
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

  private[this] def checkFieldsEquality(left: Order, right: Order): Assertion = {
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
    left.proofs shouldBe right.proofs
    left.matcherFeeAssetId shouldBe right.matcherFeeAssetId
  }
}
