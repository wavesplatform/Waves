package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange.AssetPair.extractAssetId
import com.wavesplatform.transaction.serialization.impl.ExchangeTxSerializer
import com.wavesplatform.transaction.{Asset, Proofs, TxExchangeAmount, TxMatcherFee, TxOrderPrice, TxVersion}
import com.wavesplatform.utils.JsonMatchers
import com.wavesplatform.{NTPTime, crypto}
import org.scalacheck.Gen
import play.api.libs.json.Json

import scala.math.pow

//noinspection ScalaStyle
class ExchangeTransactionSpecification extends PropSpec with NTPTime with JsonMatchers {
  val versionsGen: Gen[(Byte, Byte, Byte)] = Gen.oneOf(
    (1.toByte, 1.toByte, 1.toByte),
    (1.toByte, 2.toByte, 2.toByte),
    (1.toByte, 3.toByte, 2.toByte),
    (2.toByte, 2.toByte, 2.toByte),
    (2.toByte, 3.toByte, 2.toByte),
    (3.toByte, 3.toByte, 2.toByte)
  )

  val preconditions: Gen[(KeyPair, KeyPair, KeyPair, AssetPair, Asset, Asset, (Byte, Byte, Byte))] =
    for {
      sender1                 <- accountGen
      sender2                 <- accountGen
      matcher                 <- accountGen
      pair                    <- assetPairGen
      buyerAnotherAsset       <- assetIdGen.map(Asset.fromCompatId)
      sellerAnotherAsset      <- assetIdGen.map(Asset.fromCompatId)
      buyerMatcherFeeAssetId  <- Gen.oneOf(pair.amountAsset, pair.priceAsset, buyerAnotherAsset)
      sellerMatcherFeeAssetId <- Gen.oneOf(pair.amountAsset, pair.priceAsset, sellerAnotherAsset)
      versions                <- versionsGen
    } yield (sender1, sender2, matcher, pair, buyerMatcherFeeAssetId, sellerMatcherFeeAssetId, versions)

  property("ExchangeTransaction transaction serialization roundtrip") {
    forAll(exchangeTransactionGen) { om =>
      val recovered = ExchangeTransaction.parseBytes(om.bytes()).get
      om.id() shouldBe recovered.id()
      om.buyOrder.idStr() shouldBe recovered.buyOrder.idStr()
      recovered.bytes() shouldEqual om.bytes()
    }
  }

  property("ExchangeV1 decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "BwAAAOsAAADr9IY3xy16mJnKoO7SBmSNwHaWV3+d1vdcScPApgVWuxw/7SjoOPJrjgTy5xdT8N3r7nLDWi3CccphnxST/KmeUQHxYAG8ADfqHbsA/38AAH+keW+2fw8DAP6A/wB//21bgAHx3r0A/y7FAH8BAFS23wGAAQEBL//yAfnlgP9y/79zAAAAABG5xwQSFQAAArPyBO9qQmFjJrw/+p8AAAFukBeODgAAEkKNAYSH9FYkAkJ2Q6J9qR5OrnYtaK/sCcM9DZQ3CodGM6JrI/QnfvYXZUb+20f7cDxjOiGauEtcQsv8J0AHA2tAB5kShDOChq6mDOBwWh43hScnayT6y/MZbsruL5u9cyOjzaQFP+0o6Djya44E8ucXU/Dd6+5yw1otwnHKYZ8Uk/ypnlEB8WABvAA36h27AP9/AAB/pHlvtn8PAwD+gP8Af/9tW4AB8d69AP8uxQB/AQBUtt8BgAEBAS//8gH55YD/cv+/cwABAAARuccEEhUAAA7fHo31SkJhYya8P/qfAAABbpAXjg4AABJCjQGEhwijiz5c7B6+H7DEoTcbco5lJ8qmEul84+Ajj7xYCgdTFOnn7vjdnBQxnD3D0O5JMo4M+62X+BFJOqv17/E5DwYAABG5xwQSFQAAAACD65xwAAAAA3s1q3kAAAAAofpKAgAAAAIOl/q9AAABbpAXjap5ZjtkLKpxCBjDxPna0J5VpVVycklUVJbBaeMSZo48HqkGqYHNxTlQAEywiUxZk/7Ba60mv9//myCl4QmucpWK"
    )
    val json = Json.parse(
      """{
        |  "senderPublicKey" : "5JYRxUHTuVkHvfdt9gkfWd3hn9qW3mmqUqCkCA3PqLSU",
        |  "amount" : 2213256304,
        |  "signature" : "3RmyNspCXA1WTxKCn34tPVVDoFXAZ5upiTJNfxDovDajMHUsZQ2YcHBq3wNzAXt7znWhb6AvcySt2vqVorKdfVqB",
        |  "fee" : 8834775741,
        |  "type" : 7,
        |  "version" : 1,
        |  "sellMatcherFee" : 2717534722,
        |  "sender" : "3N9kr6BrKYCYs1tEF1c7rPXswKPyajqb86k",
        |  "feeAssetId" : null,
        |  "proofs" : [ "3RmyNspCXA1WTxKCn34tPVVDoFXAZ5upiTJNfxDovDajMHUsZQ2YcHBq3wNzAXt7znWhb6AvcySt2vqVorKdfVqB" ],
        |  "price" : 19489605554709,
        |  "id" : "5Z4B6S27X27edwWj8aYVLP2yXaeCcNPu57Q2KSje9FDY",
        |  "order2" : {
        |    "version" : 1,
        |    "id" : "BMVA9vp4vDcjwHsgGCsN5FFTiMb7yFpkGkp5Jpug4o7H",
        |    "sender" : "3MswDigq2LUsRTUpEkmjEDaQbhgmg9eTfeD",
        |    "senderPublicKey" : "4U5ENWf58FPV32uVt2LmkbQMb7bJZnTR2ZHSFEmYokma",
        |    "matcherPublicKey" : "5JYRxUHTuVkHvfdt9gkfWd3hn9qW3mmqUqCkCA3PqLSU",
        |    "assetPair" : {
        |      "amountAsset" : "HFEAWpxTBVhiZfyLn9LRnCRGmz8YT61Ah99he1y6MJd5",
        |      "priceAsset" : "HHAFGgAJfFKeYyJJJNMgtmWbeLCycUunUq2wuRgsnSgX"
        |    },
        |    "orderType" : "sell",
        |    "amount" : 16351453115722,
        |    "price" : 19489605554709,
        |    "timestamp" : 4783213297262394015,
        |    "expiration" : 1574375493134,
        |    "matcherFee" : 20077042828423,
        |    "signature" : "B22PFqHfHPxXesjvqQBmjd4BRd2KCNXveMutM5WEvrQ8P6WW7BcDGoC3jfEZ92Bj25dBgem9KbbDijXGrm8WXRf",
        |    "proofs" : [ "B22PFqHfHPxXesjvqQBmjd4BRd2KCNXveMutM5WEvrQ8P6WW7BcDGoC3jfEZ92Bj25dBgem9KbbDijXGrm8WXRf" ]
        |  },
        |  "order1" : {
        |    "version" : 1,
        |    "id" : "5bZMswwfyY2LMXhTE3s3Ng1wA7ckfkMYMJeqFz7bTXf3",
        |    "sender" : "3NCyrQZdTZARUgXhia2rJzkJ1A73qMSefDA",
        |    "senderPublicKey" : "HTXBWf7iBQgHm9fSpHwjLff3W9NQm7Mi822vGWJGtbPH",
        |    "matcherPublicKey" : "5JYRxUHTuVkHvfdt9gkfWd3hn9qW3mmqUqCkCA3PqLSU",
        |    "assetPair" : {
        |      "amountAsset" : "HFEAWpxTBVhiZfyLn9LRnCRGmz8YT61Ah99he1y6MJd5",
        |      "priceAsset" : "HHAFGgAJfFKeYyJJJNMgtmWbeLCycUunUq2wuRgsnSgX"
        |    },
        |    "orderType" : "buy",
        |    "amount" : 2971882811242,
        |    "price" : 19489605554709,
        |    "timestamp" : 4783213297262394015,
        |    "expiration" : 1574375493134,
        |    "matcherFee" : 20077042828423,
        |    "signature" : "5tLNaVttbCwLLSmSfZkVRxvE6HML3DPrFmXXJm7Y9TsfAgZ4cNRpxxkkVcAHNnJRJtQZHtALmetSAbLMmsivhmWK",
        |    "proofs" : [ "5tLNaVttbCwLLSmSfZkVRxvE6HML3DPrFmXXJm7Y9TsfAgZ4cNRpxxkkVcAHNnJRJtQZHtALmetSAbLMmsivhmWK" ]
        |  },
        |  "buyMatcherFee" : 14952016761,
        |  "timestamp" : 1574375493034
        |}
        |""".stripMargin
    )

    val tx = ExchangeTxSerializer.parseBytes(bytes).get
    tx.json() should matchJson(json)
    assert(crypto.verify(tx.sellOrder.signature, tx.sellOrder.bodyBytes(), tx.sellOrder.sender), "sellOrder signature should be valid")
    assert(crypto.verify(tx.buyOrder.signature, tx.buyOrder.bodyBytes(), tx.buyOrder.sender), "buyOrder signature should be valid")
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("ExchangeV2 decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "AAcCAAAA8QK+o7m/j7ZEi+lM2oE0cec3t4lb556CcDbjgPXmAQRxVSX9uNZTQxi/BAK0J0+qRUYyWeWwFJcFYNBaSxQ0GjcyAQAAgADf/wAAf4D/a7wG0f8BoGEBfwHff4D//+x/f94BAQCAcf8AgBz/AAH/X38B2lIjUxT//2///4D/gP9WABqAAAAAAUBccJzlAAAVkbhXuJVZ/bUwsm9ykgAAAW75rITfAAAZgUnEb3EBAAEAQFSX8+znQylR4qI1eWdKXT04bVgH/W+npUEH7pYV07i+/5AqmQMfqbSCzPKCHBMUx/bij/vlbn8RBTZsyHm7zQIAAADyAzSq2tCPs5dl4m4HlMSvIyWRyJHUFuxGjWJsqHImxZgXJf241lNDGL8EArQnT6pFRjJZ5bAUlwVg0FpLFDQaNzIBAACAAN//AAB/gP9rvAbR/wGgYQF/Ad9/gP//7H9/3gEBAIBx/wCAHP8AAf9ffwHaUiNTFP//b///gP+A/1YAGoABAAABQFxwnOUAABdqPljMsln9tTCyb3KSAAABbvmshN8AABmBScRvcQABAAEAQGDF9E4Yz7DdMiQMOX2kqIqHYig4J3DxRuQHNgZgkVQyuDEwSyTEFzfVv+SgaCTT9q3oZgO1IHDbskkcD0LyngcAAAFAXHCc5QAAAANnzytVAAAABAbkM9UAAAADtaBwygAAAAPeQlJPAAABbvmshHsBAAEAQPL1jLp5HAdsUrnxa3cdRRip8t1UGEqQbbEBXRT4/CrlgAlK+EO4Cw1Dz0xLEG1retjDt2LfIVwR10MEN1thqQs="
    )
    val json = Json.parse(
      """{
        |  "senderPublicKey" : "3ZJUiWs5xiuZqYBHBthdgH4U6tYxLy5fHNKxMApEMJ6V",
        |  "amount" : 14626532181,
        |  "fee" : 16613790287,
        |  "type" : 7,
        |  "version" : 2,
        |  "sellMatcherFee" : 15932092618,
        |  "sender" : "3Mubc9sE9PSTUb2xNUrayMcQEbTQTFuwFvV",
        |  "feeAssetId" : null,
        |  "proofs" : [ "5rjjpMEqcv6Dpc6tUukWCPNLKWFbtALPwpfKTHYmNS2x1xSZyJ7UCHqEGHG2ANuyuYT831wTxdqTFyhanbD9bVSa" ],
        |  "price" : 1375940418789,
        |  "id" : "EwHU7KoCxXHadkLgooJNt7guvb4v7UWV6ZmSv5AZ2XPN",
        |  "order2" : {
        |    "senderPublicKey" : "4YbJhJxb6CbdjCx6T2Bp9J9zAy8tjr58BYpZkFCqqMuY",
        |    "orderType" : "sell",
        |    "amount" : 25745079979186,
        |    "matcherFeeAssetId" : null,
        |    "signature" : "2wDh9Jgf42R9ahTE4Sg2pVChPX9Yti5epKBFsrphEG96RQEJriptHUyofgxy6Hz8CuTrGAKyPMjPQmUEQwd41k6i",
        |    "assetPair" : {
        |      "amountAsset" : "11SecKqegK2kMMorte6oN1qsp19B85CxGMGqYys9cJU",
        |      "priceAsset" : "12xbfZzJThEirK9zGRyvmhqNSHmvzkroiU3GkVfCUDtT"
        |    },
        |    "version" : 3,
        |    "matcherFee" : 28043079085937,
        |    "sender" : "3Mqo7jsWkw6ncJm1Q1BkpUQRmtsJiWz72dC",
        |    "price" : 1375940418789,
        |    "proofs" : [ "2wDh9Jgf42R9ahTE4Sg2pVChPX9Yti5epKBFsrphEG96RQEJriptHUyofgxy6Hz8CuTrGAKyPMjPQmUEQwd41k6i" ],
        |    "matcherPublicKey" : "3ZJUiWs5xiuZqYBHBthdgH4U6tYxLy5fHNKxMApEMJ6V",
        |    "expiration" : 1576146863327,
        |    "id" : "ARayJvx7uq9gLnXoMYAbUqE13LD7a367hsR58ALUMu86",
        |    "timestamp" : 6484538259240088210
        |  },
        |  "order1" : {
        |    "senderPublicKey" : "DqBGDQE5XafzqV2ZpL4FxnTR8ECbqT5pekhHU5TrTAdA",
        |    "orderType" : "buy",
        |    "amount" : 23715607197845,
        |    "signature" : "2h6XD9GHUh1Cmy65pdFrVvJkqRD3hQehnR7ysGQwH3kUoh7oD7eQaJWNL9fENfrSXgqjV7uESrPDeYwFi5aD2A49",
        |    "assetPair" : {
        |      "amountAsset" : "11SecKqegK2kMMorte6oN1qsp19B85CxGMGqYys9cJU",
        |      "priceAsset" : "12xbfZzJThEirK9zGRyvmhqNSHmvzkroiU3GkVfCUDtT"
        |    },
        |    "version" : 2,
        |    "matcherFee" : 28043079085937,
        |    "sender" : "3MrqxMUe7mCKnJNgJjmWWbuzJQgQuLVKhkp",
        |    "price" : 1375940418789,
        |    "proofs" : [ "2h6XD9GHUh1Cmy65pdFrVvJkqRD3hQehnR7ysGQwH3kUoh7oD7eQaJWNL9fENfrSXgqjV7uESrPDeYwFi5aD2A49" ],
        |    "matcherPublicKey" : "3ZJUiWs5xiuZqYBHBthdgH4U6tYxLy5fHNKxMApEMJ6V",
        |    "expiration" : 1576146863327,
        |    "id" : "7zS6RWD9BAogytjd3mMtVxDubFbR97gnFzKT7TifRU4y",
        |    "timestamp" : 6484538259240088210
        |  },
        |  "buyMatcherFee" : 17295487957,
        |  "timestamp" : 1576146863227
        |}
        |
        |""".stripMargin
    )

    val tx = ExchangeTxSerializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.sellOrder.signature, tx.sellOrder.bodyBytes(), tx.sellOrder.sender), "sellOrder signature should be valid")
    assert(crypto.verify(tx.buyOrder.signature, tx.buyOrder.bodyBytes(), tx.buyOrder.sender), "buyOrder signature should be valid")
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("ExchangeTransaction invariants validation") {

    forAll(preconditions) { case (sender1, sender2, matcher, pair, buyerMatcherFeeAssetId, sellerMatcherFeeAssetId, versions) =>
      val time                = ntpTime.correctedTime()
      val expirationTimestamp = time + Order.MaxLiveTime / 2

      val buyPrice       = 60 * Order.PriceConstant
      val sellPrice      = 50 * Order.PriceConstant
      val buyAmount      = 2
      val sellAmount     = 3
      val buyMatcherFee  = 1
      val sellMatcherFee = 2

      val (buyV, sellV, exchangeV) = versions

      val buy = Order
        .buy(
          buyV,
          sender1,
          matcher.publicKey,
          pair,
          buyAmount,
          buyPrice,
          time,
          expirationTimestamp,
          buyMatcherFee,
          if (buyV == 3) buyerMatcherFeeAssetId else Waves
        )
        .explicitGet()
      val sell = Order
        .sell(
          sellV,
          sender2,
          matcher.publicKey,
          pair,
          sellAmount,
          sellPrice,
          time,
          expirationTimestamp,
          sellMatcherFee,
          if (sellV == 3) sellerMatcherFeeAssetId else Waves
        )
        .explicitGet()

      def create(
          matcher: KeyPair = sender1,
          buyOrder: Order = buy,
          sellOrder: Order = sell,
          amount: Long = buyAmount,
          price: Long = sellPrice,
          buyMatcherFee: Long = buyMatcherFee,
          sellMatcherFee: Long = 1,
          fee: Long = 1,
          timestamp: Long = expirationTimestamp - Order.MaxLiveTime,
          version: Byte = exchangeV
      ): Either[ValidationError, ExchangeTransaction] = {
        if (version == 1) {
          ExchangeTransaction.signed(
            1.toByte,
            matcher = matcher.privateKey,
            order1 = buyOrder,
            order2 = sellOrder,
            amount = amount,
            price = price,
            buyMatcherFee = buyMatcherFee,
            sellMatcherFee = sellMatcherFee,
            fee = fee,
            timestamp = timestamp
          )
        } else {
          ExchangeTransaction.signed(
            version,
            matcher = matcher.privateKey,
            order1 = buyOrder,
            order2 = sellOrder,
            amount = amount,
            price = price,
            buyMatcherFee = buyMatcherFee,
            sellMatcherFee = sellMatcherFee,
            fee = fee,
            timestamp = timestamp
          )
        }
      }

      buy.version shouldBe buyV
      sell.version shouldBe sellV

      create() shouldBe an[Right[_, _]]
      create(fee = pow(10, 18).toLong) shouldBe an[Right[_, _]]
      create(amount = Order.MaxAmount) shouldBe an[Right[_, _]]

      create(fee = -1) shouldBe an[Left[_, _]]
      create(amount = -1) shouldBe an[Left[_, _]]
      create(amount = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
      create(price = -1) shouldBe an[Left[_, _]]
      create(sellMatcherFee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
      create(buyMatcherFee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
      create(fee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]

      create(buyOrder = buy.copy(orderType = OrderType.SELL)) shouldBe Left(GenericError("order1 should have OrderType.BUY"))
      create(buyOrder = buy.copy(assetPair = buy.assetPair.copy(amountAsset = sell.assetPair.priceAsset))) shouldBe an[Left[_, _]]
      create(buyOrder = buy.copy(expiration = 1L)) shouldBe an[Left[_, _]]
      create(buyOrder = buy.copy(expiration = buy.expiration + 1)) shouldBe an[Left[_, _]]
      create(buyOrder = buy.copy(matcherPublicKey = sender2.publicKey)) shouldBe an[Left[_, _]]

      create(sellOrder = sell.copy(orderType = OrderType.BUY)) shouldBe Left(GenericError("sellOrder should has OrderType.SELL"))
      create(sellOrder = sell.copy(assetPair = sell.assetPair.copy(priceAsset = buy.assetPair.amountAsset))) shouldBe an[Left[_, _]]
      create(sellOrder = sell.copy(expiration = 1L)) shouldBe an[Left[_, _]]
      create(sellOrder = sell.copy(expiration = sell.expiration + 1)) shouldBe an[Left[_, _]]
      create(sellOrder = sell.copy(matcherPublicKey = sender2.publicKey)) shouldBe an[Left[_, _]]

      create(sellOrder = buy, buyOrder = sell) shouldBe Left(GenericError("order1 should have OrderType.BUY"))
      create(version = TxVersion.V3, sellOrder = buy, buyOrder = sell) shouldBe an[Right[_, _]]
      create(version = TxVersion.V3, sellOrder = sell, buyOrder = sell) shouldBe Left(GenericError("buyOrder should has OrderType.BUY"))
      create(version = TxVersion.V3, sellOrder = buy, buyOrder = buy) shouldBe Left(GenericError("sellOrder should has OrderType.SELL"))

      create(
        buyOrder = buy.copy(assetPair = buy.assetPair.copy(amountAsset = Waves)),
        sellOrder = sell.copy(assetPair = sell.assetPair.copy(priceAsset = IssuedAsset(ByteStr(Array(1: Byte)))))
      ) shouldBe an[Left[_, _]]
    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: KeyPair, version: TxVersion): Either[ValidationError, ExchangeTransaction] = {
    val matcherFee = 300000L
    val amount     = math.min(buy.amount.value, sell.amount.value)

    if (version == 1) {
      ExchangeTransaction.signed(
        1.toByte,
        matcher = matcher.privateKey,
        order1 = buy,
        order2 = sell,
        amount = amount,
        price = price,
        buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount.value).toLong,
        sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount.value).toLong,
        fee = matcherFee,
        timestamp = ntpTime.correctedTime()
      )
    } else {
      ExchangeTransaction.signed(
        2.toByte,
        matcher = matcher.privateKey,
        order1 = buy,
        order2 = sell,
        amount = amount,
        price = price,
        buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount.value).toLong,
        sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount.value).toLong,
        fee = matcherFee,
        timestamp = ntpTime.correctedTime()
      )
    }
  }

  property("Test transaction with small amount and expired order") {

    forAll(preconditions) { case (sender1, sender2, matcher, pair, buyerMatcherFeeAssetId, sellerMatcherFeeAssetId, versions) =>
      val time                     = ntpTime.correctedTime()
      val expirationTimestamp      = time + Order.MaxLiveTime / 2
      val buyPrice                 = 1 * Order.PriceConstant
      val sellPrice                = (0.50 * Order.PriceConstant).toLong
      val matcherFee               = 300000L
      val (sellV, buyV, exchangeV) = versions

      val sell =
        Order
          .sell(
            sellV,
            sender2,
            matcher.publicKey,
            pair,
            2,
            sellPrice,
            time,
            expirationTimestamp,
            matcherFee,
            if (sellV == 3) sellerMatcherFeeAssetId else Waves
          )
          .explicitGet()
      val buy =
        Order
          .buy(
            buyV,
            sender1,
            matcher.publicKey,
            pair,
            1,
            buyPrice,
            time,
            expirationTimestamp,
            matcherFee,
            if (buyV == 3) buyerMatcherFeeAssetId else Waves
          )
          .explicitGet()

      createExTx(buy, sell, sellPrice, matcher, exchangeV) shouldBe an[Right[_, _]]

      val sell1 =
        if (sellV == 3) {
          Order.sell(sellV, sender2, matcher.publicKey, pair, 1, buyPrice, time, time - 1, matcherFee, sellerMatcherFeeAssetId).explicitGet()
        } else Order.sell(sellV, sender2, matcher.publicKey, pair, 1, buyPrice, time, time - 1, matcherFee).explicitGet()

      createExTx(buy, sell1, buyPrice, matcher, exchangeV) shouldBe Left(OrderValidationError(sell1, "expiration should be > currentTime"))
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
         "version": 1,
         "type":7,
         "id":"FaDrdKax2KBZY6Mh7K3tWmanEdzZx6MhYUmpjV3LBJRp",
         "sender":"3N22UCTvst8N1i1XDvGHzyqdgmZgwDKbp44",
         "senderPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
         "fee":1,
         "feeAssetId": null,
         "timestamp":1526992336241,
         "signature":"5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa",
         "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
         "order1":{
            "version": 1,
            "id":"EdUTcUZNK3NYKuPrsPCkZGzVUwpjx6qVjd4TgBwna7po",
            "sender":"3MthkhReCHXeaPZcWXcT3fa6ey1XWptLtwj",
            "senderPublicKey":"BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"buy",
            "price":6000000000,
            "amount":2,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":1,
            "signature":"2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs",
            "proofs":["2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs"]
         },
         "order2":{
            "version": 1,
            "id":"DS9HPBGRMJcquTb3sAGAJzi73jjMnFFSWWHfzzKK32Q7",
            "sender":"3MswjKzUBKCD6i1w4vCosQSbC8XzzdBx1mG",
            "senderPublicKey":"7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"sell",
            "price":5000000000,
            "amount":3,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":2,
            "signature":"2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq",
            "proofs":["2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq"]
         },
         "price":5000000000,
         "amount":2,
         "buyMatcherFee":1,
         "sellMatcherFee":1
      }
      """)

    val buy = Order(
      Order.V1,
      OrderAuthentication.OrderProofs(
        PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
        Proofs(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get)
      ),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(2),
      TxOrderPrice.unsafeFrom(6000000000L),
      1526992336241L,
      1529584336241L,
      TxMatcherFee.unsafeFrom(1)
    )

    val sell = Order(
      Order.V1,
      OrderAuthentication.OrderProofs(
        PublicKey.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").explicitGet(),
        Proofs(ByteStr.decodeBase58("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get)
      ),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      TxExchangeAmount.unsafeFrom(3),
      TxOrderPrice.unsafeFrom(5000000000L),
      1526992336241L,
      1529584336241L,
      TxMatcherFee.unsafeFrom(2)
    )

    val tx = ExchangeTransaction
      .create(
        TxVersion.V1,
        buy,
        sell,
        2,
        5000000000L,
        1,
        1,
        1,
        1526992336241L,
        Proofs(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get)
      )
      .explicitGet()

    js should matchJson(tx.json())
  }

  property("JSON format validation V2") {
    val js = Json.parse("""{
         "version": 2,
         "type":7,
         "id":"5KUDbPKjAoNHTMyae9zJZpFjYFAbeSQMQ9rzgkDEEUx6",
         "sender":"3N22UCTvst8N1i1XDvGHzyqdgmZgwDKbp44",
         "senderPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
         "fee":1,
         "feeAssetId": null,
         "timestamp":1526992336241,
         "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
         "order1":{
            "version": 2,
            "id":"EcndU4vU3SJ58KZAXJPKACvMhijTzgRjLTsuWxSWaQUK",
            "sender":"3MthkhReCHXeaPZcWXcT3fa6ey1XWptLtwj",
            "senderPublicKey":"BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"buy",
            "price":6000000000,
            "amount":2,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":1,
            "signature":"2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs",
            "proofs":["2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs"]
         },
         "order2":{
            "version": 1,
            "id":"DS9HPBGRMJcquTb3sAGAJzi73jjMnFFSWWHfzzKK32Q7",
            "sender":"3MswjKzUBKCD6i1w4vCosQSbC8XzzdBx1mG",
            "senderPublicKey":"7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"sell",
            "price":5000000000,
            "amount":3,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":2,
            "signature":"2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq",
            "proofs":["2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq"]
         },
         "price":5000000000,
         "amount":2,
         "buyMatcherFee":1,
         "sellMatcherFee":1
      }
      """)

    val buy = Order(
      Order.V2,
      OrderAuthentication.OrderProofs(
        PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
        Proofs(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get)
      ),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(2),
      TxOrderPrice.unsafeFrom(6000000000L),
      1526992336241L,
      1529584336241L,
      TxMatcherFee.unsafeFrom(1)
    )

    val sell = Order(
      Order.V1,
      OrderAuthentication.OrderProofs(
        PublicKey.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").explicitGet(),
        Proofs(ByteStr.decodeBase58("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get)
      ),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      TxExchangeAmount.unsafeFrom(3),
      TxOrderPrice.unsafeFrom(5000000000L),
      1526992336241L,
      1529584336241L,
      TxMatcherFee.unsafeFrom(2)
    )

    val tx = ExchangeTransaction
      .create(
        TxVersion.V2,
        buy,
        sell,
        2,
        5000000000L,
        1,
        1,
        1,
        1526992336241L,
        Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))
      )
      .explicitGet()

    js should matchJson(tx.json())
  }

  property("JSON format validation V2 OrderV3") {
    val js = Json.parse("""{
         "version": 2,
         "type":7,
         "id":"3G1U1UX2mtWXVdZTZNjEYvPeNn6cyYmmjHYUePrg4zM5",
         "sender":"3N22UCTvst8N1i1XDvGHzyqdgmZgwDKbp44",
         "senderPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
         "fee":1,
         "feeAssetId": null,
         "timestamp":1526992336241,
         "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
         "order1":{
            "version": 3,
            "id":"8KZby2jXfFCaFtEKejqBbutQvyimgeQykwPKGi3ufNiA",
            "sender":"3MthkhReCHXeaPZcWXcT3fa6ey1XWptLtwj",
            "senderPublicKey":"BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"buy",
            "price":6000000000,
            "amount":2,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":1,
            "matcherFeeAssetId":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy",
            "signature":"2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs",
            "proofs":["2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs"]
         },
         "order2":{
            "version": 1,
            "id":"DS9HPBGRMJcquTb3sAGAJzi73jjMnFFSWWHfzzKK32Q7",
            "sender":"3MswjKzUBKCD6i1w4vCosQSbC8XzzdBx1mG",
            "senderPublicKey":"7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"sell",
            "price":5000000000,
            "amount":3,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":2,
            "signature":"2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq",
            "proofs":["2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq"]
         },
         "price":5000000000,
         "amount":2,
         "buyMatcherFee":1,
         "sellMatcherFee":1
      }
      """)

    val buy = Order(
      Order.V3,
      OrderAuthentication.OrderProofs(
        PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
        Proofs(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get)
      ),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(2),
      TxOrderPrice.unsafeFrom(6000000000L),
      1526992336241L,
      1529584336241L,
      TxMatcherFee.unsafeFrom(1),
      extractAssetId("9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get
    )

    val sell = Order(
      Order.V1,
      OrderAuthentication.OrderProofs(
        PublicKey.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").explicitGet(),
        Proofs(ByteStr.decodeBase58("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get)
      ),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      TxExchangeAmount.unsafeFrom(3),
      TxOrderPrice.unsafeFrom(5000000000L),
      1526992336241L,
      1529584336241L,
      TxMatcherFee.unsafeFrom(2)
    )

    val tx = ExchangeTransaction
      .create(
        TxVersion.V2,
        buy,
        sell,
        2,
        5000000000L,
        1,
        1,
        1,
        1526992336241L,
        Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))
      )
      .explicitGet()

    js should matchJson(tx.json())
  }
}
