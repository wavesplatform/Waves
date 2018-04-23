package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state.ByteStr
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.transaction.ValidationError.OrderValidationError
import scorex.transaction.assets.exchange.{Order, _}
import scorex.utils._

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

}
