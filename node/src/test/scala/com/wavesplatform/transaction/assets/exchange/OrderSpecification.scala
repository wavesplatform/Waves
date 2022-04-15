package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.NTPTime
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderAuthentication.OrderProofs
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.transaction.{Asset, Proofs, TxExchangeAmount, TxHelpers, TxMatcherFee, TxOrderPrice, ValidationMatcher}
import org.scalatest.*

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
    val versions       = Seq(Order.V1, Order.V2, Order.V3, Order.V4)
    val sender         = TxHelpers.signer(1)
    val matcher        = TxHelpers.signer(2)
    val pair           = AssetPair(IssuedAsset(ByteStr.fill(32)(1)), IssuedAsset(ByteStr.fill(32)(2)))
    val price          = 1
    val amount         = 100
    val time           = 10000000000L
    val expirationTime = 20000000000L
    val matcherFee     = 2

    versions.foreach { version =>
      val matcherFeeAsset = if (version == 3) IssuedAsset(ByteStr.fill(32)(3)) else Waves

      val buyOrder =
        Order.buy(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, matcherFee, matcherFeeAsset).explicitGet()
      val sellOrder =
        Order.sell(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, matcherFee, matcherFeeAsset).explicitGet()

      buyOrder.isValid(30000000000L) shouldBe not(valid)
      buyOrder.isValid(expirationTime - Order.MaxLiveTime - 1) shouldBe not(valid)

      sellOrder.isValid(30000000000L) shouldBe not(valid)
      sellOrder.isValid(expirationTime - Order.MaxLiveTime - 1) shouldBe not(valid)
    }
  }

  property("Order amount validation") {
    val versions       = Seq(Order.V1, Order.V2, Order.V3, Order.V4)
    val sender         = TxHelpers.signer(1)
    val matcher        = TxHelpers.signer(2)
    val pair           = AssetPair(IssuedAsset(ByteStr.fill(32)(1)), IssuedAsset(ByteStr.fill(32)(2)))
    val price          = 1
    val time           = 1000
    val expirationTime = 2000
    val matcherFee     = 2

    versions.foreach { version =>
      val matcherFeeAsset = if (version == 3) IssuedAsset(ByteStr.fill(32)(3)) else Waves

      Order.buy(version, sender, matcher.publicKey, pair, 0, price, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft
      Order.buy(version, sender, matcher.publicKey, pair, -1, price, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft
      Order.buy(version, sender, matcher.publicKey, pair, Order.MaxAmount + 1, price, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft

      Order.sell(version, sender, matcher.publicKey, pair, 0, price, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft
      Order.sell(version, sender, matcher.publicKey, pair, -1, price, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft
      Order.sell(
        version,
        sender,
        matcher.publicKey,
        pair,
        Order.MaxAmount + 1,
        price,
        time,
        expirationTime,
        matcherFee,
        matcherFeeAsset
      ) should beLeft
    }
  }

  property("Order matcherFee validation") {
    val versions       = Seq(Order.V1, Order.V2, Order.V3, Order.V4)
    val sender         = TxHelpers.signer(1)
    val matcher        = TxHelpers.signer(2)
    val pair           = AssetPair(IssuedAsset(ByteStr.fill(32)(1)), IssuedAsset(ByteStr.fill(32)(2)))
    val price          = 1
    val amount         = 100
    val time           = 1000
    val expirationTime = 2000

    versions.foreach { version =>
      val matcherFeeAsset = if (version == 3) IssuedAsset(ByteStr.fill(32)(3)) else Waves

      Order.buy(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, 0, matcherFeeAsset) should beLeft
      Order.buy(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, -1, matcherFeeAsset) should beLeft
      Order.buy(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, Order.MaxAmount + 1, matcherFeeAsset) should beLeft

      Order.sell(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, 0, matcherFeeAsset) should beLeft
      Order.sell(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, -1, matcherFeeAsset) should beLeft
      Order.sell(version, sender, matcher.publicKey, pair, amount, price, time, expirationTime, Order.MaxAmount + 1, matcherFeeAsset) should beLeft
    }
  }

  property("Order price validation") {
    val versions       = Seq(Order.V1, Order.V2, Order.V3, Order.V4)
    val sender         = TxHelpers.signer(1)
    val matcher        = TxHelpers.signer(2)
    val pair           = AssetPair(IssuedAsset(ByteStr.fill(32)(1)), IssuedAsset(ByteStr.fill(32)(2)))
    val amount         = 100
    val time           = 1000
    val expirationTime = 2000
    val matcherFee     = 2

    versions.foreach { version =>
      val matcherFeeAsset = if (version == 3) IssuedAsset(ByteStr.fill(32)(3)) else Waves

      Order.buy(version, sender, matcher.publicKey, pair, amount, 0, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft
      Order.buy(version, sender, matcher.publicKey, pair, amount, -1, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft

      Order.sell(version, sender, matcher.publicKey, pair, amount, 0, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft
      Order.sell(version, sender, matcher.publicKey, pair, amount, -1, time, expirationTime, matcherFee, matcherFeeAsset) should beLeft
    }
  }

  property("Order signature validation") {
    val err = "Proof doesn't validate as signature"
    forAll(orderGen, accountGen) { case (order, pka) =>
      val rndAsset = Array[Byte](32)

      Random.nextBytes(rndAsset)

      Verifier.verifyAsEllipticCurveSignature(order, checkWeakPk = true) should beRight

      Verifier.verifyAsEllipticCurveSignature(order.copy(matcherPublicKey = pka.publicKey), checkWeakPk = true) should produce(err)
      val assetPair = order.assetPair
      Verifier.verifyAsEllipticCurveSignature(
        order.copy(assetPair = assetPair.copy(amountAsset = IssuedAsset(ByteStr(rndAsset)))),
        checkWeakPk = true
      ) should produce(err)
      Verifier.verifyAsEllipticCurveSignature(
        order.copy(assetPair = assetPair.copy(priceAsset = IssuedAsset(ByteStr(rndAsset)))),
        checkWeakPk = true
      ) should produce(err)
      Verifier.verifyAsEllipticCurveSignature(order.copy(orderType = OrderType.reverse(order.orderType)), checkWeakPk = true) should produce(err)
      Verifier.verifyAsEllipticCurveSignature(order.copy(price = TxOrderPrice.unsafeFrom(order.price.value + 1)), checkWeakPk = true) should produce(
        err
      )
      Verifier.verifyAsEllipticCurveSignature(
        order.copy(amount = TxExchangeAmount.unsafeFrom(order.amount.value + 1)),
        checkWeakPk = true
      ) should produce(err)
      Verifier.verifyAsEllipticCurveSignature(order.copy(expiration = order.expiration + 1), checkWeakPk = true) should produce(err)
      Verifier.verifyAsEllipticCurveSignature(
        order.copy(matcherFee = TxMatcherFee.unsafeFrom(order.matcherFee.value + 1)),
        checkWeakPk = true
      ) should produce(err)

      val orderAuth = order.orderAuthentication.asInstanceOf[OrderProofs]
      Verifier.verifyAsEllipticCurveSignature(
        order.copy(orderAuthentication = orderAuth.copy(key = pka.publicKey)),
        checkWeakPk = true
      ) should produce(err)
      Verifier.verifyAsEllipticCurveSignature(
        order.copy(orderAuthentication = orderAuth.copy(proofs = Proofs(Seq(ByteStr(pka.publicKey.arr ++ pka.publicKey.arr))))),
        checkWeakPk = true
      ) should produce(err)
    }
  }

  property("Buy and Sell orders") {
    forAll(orderParamGen) { case (sender, matcher, pair, _, amount, price, timestamp, _, _) =>
      val expiration = timestamp + Order.MaxLiveTime - 1000
      val buy = Order
        .buy(
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
        .explicitGet()
      buy.orderType shouldBe OrderType.BUY

      val sell = Order
        .sell(
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
        .explicitGet()
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
