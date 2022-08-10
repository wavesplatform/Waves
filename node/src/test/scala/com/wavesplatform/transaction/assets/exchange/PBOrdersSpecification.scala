package com.wavesplatform.transaction.assets.exchange

import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.utils.*
import com.wavesplatform.protobuf.order.AssetPair as PBAssetPair
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBOrder, PBOrders}
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.Verifier

class PBOrdersSpecification extends FlatSpec {
  private[this] val protoOrder = PBOrder(
    AddressScheme.current.chainId.toInt,
    ByteString.copyFrom(TestValues.keyPair.publicKey.arr),
    Some(PBAssetPair(PBAmounts.toPBAssetId(TestValues.asset), PBAmounts.toPBAssetId(Waves))),
    PBOrder.Side.SELL,
    amount = 1000,
    price = 1000,
    timestamp = 1000,
    expiration = 10000,
    matcherFee = Some(PBAmounts.fromAssetAndAmount(Waves, 300000L)),
    version = 1,
    proofs = Nil,
    sender = PBOrder.Sender.SenderPublicKey(ByteString.copyFrom(TestValues.keyPair.publicKey.arr))
  )

  it should "validate asset pair" in {
    val doubleAssetPair = PBAssetPair(PBAmounts.toPBAssetId(TestValues.asset), PBAmounts.toPBAssetId(TestValues.asset))
    validate(protoOrder.withAssetPair(doubleAssetPair)).toEither shouldBe Left("Invalid AssetPair")
  }

  it should "validate expiration" in {
    validate(protoOrder.copy(expiration = -1)).toEither shouldBe Left("expiration should be > currentTime")
    validate(protoOrder.copy(expiration = 0)).toEither shouldBe Left("expiration should be > currentTime")
    validate(protoOrder.copy(expiration = protoOrder.timestamp + Order.MaxLiveTime + 1)).toEither shouldBe Left(
      "expiration should be earlier than 30 days"
    )
  }

  it should "validate side" in {
    val protoSellOrder = protoOrder.copy(orderSide = PBOrder.Side.SELL)
    val sellOrder      = PBOrders.vanilla(protoSellOrder).explicitGet()
    val protoBuyOrder  = protoOrder.copy(orderSide = PBOrder.Side.BUY)
    val buyOrder       = PBOrders.vanilla(protoBuyOrder).explicitGet()

    protoSellOrder.orderSide.isBuy shouldBe false
    protoSellOrder.orderSide.isSell shouldBe true
    protoBuyOrder.orderSide.isBuy shouldBe true
    protoBuyOrder.orderSide.isSell shouldBe false

    sellOrder.orderType shouldBe OrderType.SELL
    buyOrder.orderType shouldBe OrderType.BUY

    PBOrders.vanilla(protoOrder.copy(orderSide = PBOrder.Side.Unrecognized(123))) should beLeft
  }

  it should "validate version" in {
    validate(protoOrder.copy(version = 0)).toEither shouldBe Left("invalid version")
    validate(protoOrder.copy(version = 5)).toEither shouldBe Left("invalid version")
  }

  it should "validate proofs" in {
    validate(protoOrder.copy(proofs = Seq.fill[ByteString](10)(ByteString.EMPTY))).toEither shouldBe Left("Too many proofs (10), only 8 allowed")
    validate(protoOrder.copy(proofs = Seq(ByteString.copyFrom(new Array[Byte](65))))).toEither shouldBe Left(
      "Too large proof (65), must be max 64 bytes"
    )
  }

  it should "verify signature" in {
    val signed = PBOrders
      .vanilla(
        protoOrder.copy(
          proofs = Seq(ByteString.copyFrom(Base58.decode("5f5irpd67tknEkHr9GejWSC7poZGfdaZabV84GjxifxqdtMKfcU8QnhZYBQR9F54GjfTcA8a91DSAb79CTtFoxnd")))
        )
      ).explicitGet()
    Verifier.verifyAsEllipticCurveSignature(signed, true) shouldBe Symbol("right")

    val signedV4 = PBOrders
      .vanilla(
        protoOrder.copy(
          version = Order.V4,
          proofs = Seq(ByteString.copyFrom(Base58.decode("2kRQDV8TbSEVe9B2yy8XR8XijYrbxEXTvptxuCr42Vp6u1psZyEzaRj6eAb267zA2Tm5D8EGN8FTMQFGdQDcyNT8")))
        )
      ).explicitGet()

    Verifier.verifyAsEllipticCurveSignature(signedV4, true) shouldBe Symbol("right")
  }

  it should "handle roundtrip" in {
    val vanilla                = PBOrders.vanilla(protoOrder).explicitGet()
    val reserializedProtoOrder = PBOrders.protobuf(vanilla)
    reserializedProtoOrder shouldBe protoOrder
  }

  private[this] def validate(protoOrder: PBOrder): Validation = {
    val order = PBOrders.vanilla(protoOrder).explicitGet()
    order.isValid(order.timestamp)
  }
}
