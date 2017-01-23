package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.assets.exchange._
import scorex.utils._

class OrderMatchTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

    property("OrderMatch transaction serialization roundtrip") {
      forAll {x: (OrderMatch, PrivateKeyAccount) =>
        val (om, matcher) = x
        val recovered = OrderMatch.parseBytes(om.bytes).get
        om.id shouldBe recovered.id
        om.buyOrder.id shouldBe recovered.buyOrder.id
        recovered.bytes shouldEqual om.bytes
      }
    }

  property("OrderMatch balance changes") {
    forAll(accountGen, accountGen, accountGen, assetPairGen) {
      (sender1: PrivateKeyAccount, sender2: PrivateKeyAccount, matcher: PrivateKeyAccount, pair: AssetPair) =>
      val maxTime = NTP.correctedTime() + Order.MaxLiveTime
      val buyPrice = 60
      val sellPrice = 50
      val buyAmount = 2
      val sellAmount = 3
      val mf1 = 1
      val mf2 = 2

      val buy = Order.buy(sender1, matcher, pair, buyPrice, buyAmount, maxTime, mf1)
      val sell = Order.sell(sender2, matcher, pair, sellPrice, sellAmount, maxTime, mf2)

      val unsigned = OrderMatch(buy, sell, sellPrice, buyAmount, mf1, 1, 1, maxTime - Order.MaxLiveTime, Array())

      signed(unsigned, matcher).isValid(Set()) shouldBe valid
      signed(unsigned.copy(price = sellPrice + 1), matcher).isValid(Set()) shouldBe valid
      signed(unsigned.copy(price = buyPrice), matcher).isValid(Set()) shouldBe valid
      signed(unsigned.copy(amount = buyAmount + 1), matcher).isValid(Set()) should contain ("amount should be valid")
      signed(unsigned.copy(amount = buyAmount - 1, buyMatcherFee = 0, sellMatcherFee = 0), matcher).isValid(Set()) should
        contain ("buyMatcherFee should be valid")
      signed(unsigned.copy(buyMatcherFee = mf1 + 1), matcher).isValid(Set()) should contain ("buyMatcherFee should be valid")
      signed(unsigned.copy(sellMatcherFee = mf2 + 1), matcher).isValid(Set()) should contain ("sellMatcherFee should be valid")
      signed(unsigned.copy(fee = 0), matcher).isValid(Set()) should contain ("fee should be > 0")
      signed(unsigned.copy(fee = -1), matcher).isValid(Set()) should contain ("fee should be > 0")
      signed(unsigned.copy(fee = 4), matcher).isValid(Set()) shouldBe valid
      signed(unsigned.copy(timestamp = maxTime + 1), matcher).isValid(Set())should contain ("buyOrder maxTimestamp should be > currentTime")
      signed(unsigned.copy(timestamp = maxTime), matcher).isValid(Set())  shouldBe valid
      signed(unsigned.copy(timestamp = maxTime - 1), matcher).isValid(Set())  shouldBe valid
    }
  }

  def signed(unsigned: OrderMatch, matcher: PrivateKeyAccount): OrderMatch = {
    val sig = EllipticCurveImpl.sign(matcher, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  property("OrderMatch price <= buy.price and price >= sell.price") {
    forAll(assetPairGen, accountGen, accountGen, accountGen) {
      (pair: AssetPair, sender1: PrivateKeyAccount, sender2: PrivateKeyAccount, matcher: PrivateKeyAccount) =>
        val maxtTime = NTP.correctedTime() + Order.MaxLiveTime
        val buyPrice = 100
        val sellPrice = 90
        val buyAmount = 2
        val sellAmount = 3
        val mf1 = 10
        val mf2 = 10

        val buy = Order.buy(sender1, matcher, pair, buyPrice, buyAmount, maxtTime, mf1)
        val sell = Order.sell(sender2, matcher, pair, sellPrice, sellAmount, maxtTime, mf2)

        val unsigned = OrderMatch(buy, sell, buyPrice, buyAmount, mf1, mf2*2/3, 1, maxtTime - Order.MaxLiveTime, Array())
        signed(unsigned, matcher).isValid(Set()) shouldBe valid
        signed(unsigned.copy(price = sellPrice), matcher).isValid(Set()) shouldBe valid
        signed(unsigned.copy(price = sellPrice + 1), matcher).isValid(Set()) shouldBe valid
        signed(unsigned.copy(price = (buyPrice + sellPrice)/2), matcher).isValid(Set()) shouldBe valid
        signed(unsigned.copy(price = buyPrice + 1), matcher).isValid(Set()) should contain ("price should be valid")
        signed(unsigned.copy(price = buyPrice + 1), matcher).isValid(Set()) should contain ("price should be valid")
        signed(unsigned.copy(price = sellPrice - 1), matcher).isValid(Set()) should contain ("price should be valid")
    }
  }


  property("OrderMatch matcher fee for 2 steps partially executed orders") {
    forAll(maxWavesAnountGen, maxWavesAnountGen, maxWavesAnountGen, maxWavesAnountGen, maxWavesAnountGen) {
      (buyAmount: Long, sellAmount: Long, mf1: Long, mf2: Long, mf3: Long) =>
        whenever (buyAmount < sellAmount && BigInt(mf2) * buyAmount / sellAmount > 0) {
          var pairOption = Option.empty[AssetPair]
          while(pairOption.isEmpty) {
            pairOption = assetPairGen.sample
          }
          val pair = pairOption.get
          val sender1 = accountGen.sample.get
          val sender2 = accountGen.sample.get
          val matcher = accountGen.sample.get
          val curTime = NTP.correctedTime()

          val buyPrice = sellAmount
          val sellPrice = buyAmount

          val buy1 = Order.buy(sender1, matcher, pair, buyPrice, buyAmount, curTime, mf1)
          val sell = Order.sell(sender2, matcher, pair, sellPrice, sellAmount, curTime, mf2)

          val om1 = OrderMatch(buy1, sell, buyPrice, buyAmount, mf1,
            (BigInt(mf2) * buyAmount / BigInt(sellAmount)).toLong, 1, curTime, Array())
          signed(om1, matcher).isValid(Set()) shouldBe valid

          val om1Invalid = OrderMatch(buy1, sell, buyPrice, buyAmount, mf1, mf2, 1, curTime, Array())
          signed(om1Invalid, matcher).isValid(Set()) should contain ("sellMatcherFee should be valid")

          val buy2 = Order.buy(sender1, matcher, pair, buyPrice, sellAmount - buyAmount, curTime, mf3)
          val om2 = OrderMatch(buy2, sell, buyPrice, sellAmount - om1.amount,
            mf3, mf2 - (BigInt(mf2) * buyAmount / sellAmount).toLong, 1, curTime, Array())
          signed(om2, matcher).isValid(Set(om1)) shouldBe valid

          // we should spent all fee
          val om2Invalid = OrderMatch(buy2, sell, buyPrice, sellAmount - om1.amount,
            mf3, (mf2 - (BigInt(mf2) * buyAmount / sellAmount).toLong) + 1, 1, curTime, Array())
          signed(om2Invalid, matcher).isValid(Set(om1)) should contain ("sellMatcherFee should be valid")
        }
    }
  }

  property("OrderMatch with Validations") {
    forAll {x: (OrderMatch, PrivateKeyAccount) =>
      val (om, matcher) = x
      om.isValid(Set()) shouldBe valid
      om.isValid(Set(om)) shouldBe not(valid)

      signed(om.copy(amount = -1), matcher).isValid(Set()) should contain ("amount should be > 0")

      val wrongMatcherFee = om.copy(buyMatcherFee = om.buyOrder.matcherFee + 1)
      signed(wrongMatcherFee, matcher).isValid(Set()) should contain ("buyMatcherFee should be valid")

      signed(om.copy(amount = 1), matcher).isValid(Set()) should contain ("buyMatcherFee should be valid")

    }
  }


}
