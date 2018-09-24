package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.transaction.ValidationError.OrderValidationError
import com.wavesplatform.transaction.assets.exchange.{Order, _}
import com.wavesplatform.utils.{Base58, NTP}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

class ExchangeTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ExchangeTransaction transaction serialization roundtrip") {
    forAll(exchangeTransactionGen) { om =>
      val recovered = ExchangeTransaction.parseBytes(om.bytes()).get
      om.id() shouldBe recovered.id()
      om.buyOrder.id() shouldBe recovered.buyOrder.id()
      recovered.bytes() shouldEqual om.bytes()
    }
  }

  property("ExchangeTransaction balance changes") {
    forAll(accountGen, accountGen, accountGen, assetPairGen) {
      (sender1: PrivateKeyAccount, sender2: PrivateKeyAccount, matcher: PrivateKeyAccount, pair: AssetPair) =>
        val time                = NTP.correctedTime()
        val expirationTimestamp = time + Order.MaxLiveTime
        val buyPrice            = 60 * Order.PriceConstant
        val sellPrice           = 50 * Order.PriceConstant
        val buyAmount           = 2
        val sellAmount          = 3
        val mf1                 = 1
        val mf2                 = 2

        val buy  = Order.buy(sender1, matcher, pair, buyPrice, buyAmount, time, expirationTimestamp, mf1)
        val sell = Order.sell(sender2, matcher, pair, sellPrice, sellAmount, time, expirationTimestamp, mf2)

        def create(matcher: PrivateKeyAccount = sender1,
                   buyOrder: Order = buy,
                   sellOrder: Order = sell,
                   price: Long = sellPrice,
                   amount: Long = buyAmount,
                   buyMatcherFee: Long = mf1,
                   sellMatcherFee: Long = 1,
                   fee: Long = 1,
                   timestamp: Long = expirationTimestamp - Order.MaxLiveTime) = {
          ExchangeTransaction.create(
            matcher = sender1,
            buyOrder = buyOrder,
            sellOrder = sellOrder,
            price = price,
            amount = amount,
            buyMatcherFee = buyMatcherFee,
            sellMatcherFee = sellMatcherFee,
            fee = fee,
            timestamp = timestamp
          )
        }

        create() shouldBe an[Right[_, _]]

        create(fee = -1) shouldBe an[Left[_, _]]
        create(amount = -1) shouldBe an[Left[_, _]]
        create(price = -1) shouldBe an[Left[_, _]]
        create(amount = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(sellMatcherFee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(buyMatcherFee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(fee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(buyOrder = buy.copy(matcherPublicKey = sender2)) shouldBe an[Left[_, _]]
        create(sellOrder = buy.copy(matcherPublicKey = sender2)) shouldBe an[Left[_, _]]
        create(
          buyOrder = buy.copy(assetPair = buy.assetPair.copy(amountAsset = None)),
          sellOrder = sell.copy(assetPair = sell.assetPair.copy(priceAsset = Some(ByteStr(Array(1: Byte)))))
        ) shouldBe an[Left[_, _]]
        create(buyOrder = buy.copy(expiration = 1L)) shouldBe an[Left[_, _]]
        create(price = buy.price + 1) shouldBe an[Left[_, _]]
        create(price = sell.price - 1) shouldBe an[Left[_, _]]
        create(sellOrder = sell.copy(amount = -1)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.copy(amount = -1)) shouldBe an[Left[_, _]]

    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: PrivateKeyAccount): Either[ValidationError, ExchangeTransaction] = {
    val mf     = 300000L
    val amount = math.min(buy.amount, sell.amount)
    ExchangeTransaction.create(
      matcher = matcher,
      buyOrder = buy,
      sellOrder = sell,
      price = price,
      amount = amount,
      buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
      sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
      fee = mf,
      timestamp = NTP.correctedTime()
    )
  }

  property("Test transaction with small amount and expired order") {
    forAll(accountGen, accountGen, accountGen, assetPairGen) {
      (sender1: PrivateKeyAccount, sender2: PrivateKeyAccount, matcher: PrivateKeyAccount, pair: AssetPair) =>
        val time                = NTP.correctedTime()
        val expirationTimestamp = time + Order.MaxLiveTime
        val buyPrice            = 1 * Order.PriceConstant
        val sellPrice           = (0.50 * Order.PriceConstant).toLong
        val mf                  = 300000L

        val sell = Order.sell(sender2, matcher, pair, sellPrice, 2, time, expirationTimestamp, mf)
        val buy  = Order.buy(sender1, matcher, pair, buyPrice, 1, time, expirationTimestamp, mf)

        createExTx(buy, sell, sellPrice, matcher) shouldBe an[Right[_, _]]

        val sell1 = Order.sell(sender1, matcher, pair, buyPrice, 1, time, time - 1, mf)
        createExTx(buy, sell1, buyPrice, matcher) shouldBe Left(OrderValidationError(sell1, "expiration should be > currentTime"))
    }
  }

  property("JSON format validation") {
    val js = Json.parse(
      """{
         "type":7,
         "id":"FaDrdKax2KBZY6Mh7K3tWmanEdzZx6MhYUmpjV3LBJRp",
         "sender":"3N22UCTvst8N1i1XDvGHzyqdgmZgwDKbp44",
         "senderPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
         "fee":1,
         "timestamp":1526992336241,
         "signature":"5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa",
         "order1":{"id":"EdUTcUZNK3NYKuPrsPCkZGzVUwpjx6qVjd4TgBwna7po","sender":"3MthkhReCHXeaPZcWXcT3fa6ey1XWptLtwj","senderPublicKey":"BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ","matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP","assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},"orderType":"buy","price":6000000000,"amount":2,"timestamp":1526992336241,"expiration":1529584336241,"matcherFee":1,"signature":"2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs"},
         "order2":{"id":"DS9HPBGRMJcquTb3sAGAJzi73jjMnFFSWWHfzzKK32Q7","sender":"3MswjKzUBKCD6i1w4vCosQSbC8XzzdBx1mG","senderPublicKey":"7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV","matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP","assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},"orderType":"sell","price":5000000000,"amount":3,"timestamp":1526992336241,"expiration":1529584336241,"matcherFee":2,"signature":"2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq"},
         "price":5000000000,
         "amount":2,
         "buyMatcherFee":1,
         "sellMatcherFee":1
         }
      """.stripMargin)

    val buy = Order(
      PublicKeyAccount.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
      PublicKeyAccount.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.BUY,
      6000000000L,
      2,
      1526992336241L,
      1529584336241L,
      1,
      Base58.decode("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get
    )

    val sell = Order(
      PublicKeyAccount.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").explicitGet(),
      PublicKeyAccount.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      5000000000L,
      3,
      1526992336241L,
      1529584336241L,
      2,
      Base58.decode("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get
    )

    val tx = ExchangeTransaction
      .create(
        buy,
        sell,
        5000000000L,
        2,
        1,
        1,
        1,
        1526992336241L,
        ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
