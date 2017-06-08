package com.wavesplatform.matcher

import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.model.{BuyLimitOrder, SellLimitOrder}
import com.wavesplatform.state2.ByteStr
import org.scalacheck.{Arbitrary, Gen}
import scorex.account.PrivateKeyAccount
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.utils.{NTP}

trait MatcherTestData {
  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val WalletSeed = ByteStr("Matcher".getBytes())
  val MatcherSeed = SecureCryptographicHash(Bytes.concat(Ints.toByteArray(0), WalletSeed.arr))
  val MatcherAccount = PrivateKeyAccount(MatcherSeed)
  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  val wavesAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]] = Gen.frequency((1, wavesAssetGen), (10, bytes32gen.map(Some(_))))

  val assetPairGen = Gen.zip(assetIdGen, assetIdGen).
    suchThat(p => p._1 != p._2).
    map(p => AssetPair(p._1.map(ByteStr(_)), p._2.map(ByteStr(_))))

  val maxTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
  val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(NTP.correctedTime() - _)

  val config = ConfigFactory.parseString(
    """
      |waves {
      |  directory: "/tmp/waves-test"
      |  matcher {
      |    enable: yes
      |    account: ""
      |    bind-address: "127.0.0.1"
      |    port: 6886
      |    min-order-fee: 100000
      |    order-match-tx-fee: 100000
      |    journal-directory: ${waves.directory}"/journal"
      |    snapshots-directory: ${waves.directory}"/snapshots"
      |    snapshots-interval: 1d
      |    max-open-orders: 1000
      |    price-assets: ["BASE1", "BASE2"]
      |    predefined-pairs: [{amountAsset = "BASE2", priceAsset = "BASE1"}]
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()

  val matcherSettings = MatcherSettings.fromConfig(config)

  def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) {
      value = gen.sample
    }
    value.get
  }

  val maxWavesAmountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  def buyGenerator(pair: AssetPair, price: Long, amount: Long, sender: Option[PrivateKeyAccount] = None,
                   matcherFee: Option[Long] = None): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def sellGenerator(pair: AssetPair, price: Long, amount: Long, sender: Option[PrivateKeyAccount] = None,
                    matcherFee: Option[Long] = None): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def buy(pair: AssetPair, price: BigDecimal, amount: Long, sender: Option[PrivateKeyAccount] = None,
          matcherFee: Option[Long] = None): Order =
    valueFromGen(buyGenerator(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee))._1

  def sell(pair: AssetPair, price: BigDecimal, amount: Long,sender: Option[PrivateKeyAccount] = None,
           matcherFee: Option[Long] = None): Order =
    valueFromGen(sellGenerator(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee))._1

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    orderType <- orderTypeGenerator
    price: Long <- maxWavesAmountGen
    amount: Long <- maxWavesAmountGen
    timestamp: Long <- createdTimeGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAmountGen
  } yield (Order(sender, MatcherAccount, pair, orderType, price, amount, timestamp, expiration, matcherFee), sender)

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    price: Long <- maxWavesAmountGen
    amount: Long <- maxWavesAmountGen
    timestamp: Long <- createdTimeGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAmountGen
  } yield BuyLimitOrder(price, amount, Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    price: Long <- maxWavesAmountGen
    amount: Long <- maxWavesAmountGen
    timestamp: Long <- createdTimeGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAmountGen
  } yield SellLimitOrder(price, amount, Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee))

}
