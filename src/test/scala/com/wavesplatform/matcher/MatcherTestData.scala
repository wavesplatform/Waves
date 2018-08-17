package com.wavesplatform.matcher

import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.crypto
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.matcher.model.{BuyLimitOrder, SellLimitOrder}
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.state.ByteStr
import org.scalacheck.{Arbitrary, Gen}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.utils.NTP
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

trait MatcherTestData {
  private val signatureSize = 32

  val bytes32gen: Gen[Array[Byte]]       = Gen.listOfN(signatureSize, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val WalletSeed                         = ByteStr("Matcher".getBytes())
  val MatcherSeed                        = crypto.secureHash(Bytes.concat(Ints.toByteArray(0), WalletSeed.arr))
  val MatcherAccount                     = PrivateKeyAccount(MatcherSeed)
  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long]         = Gen.choose(1, Long.MaxValue)

  val wavesAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]]    = Gen.frequency((1, wavesAssetGen), (10, bytes32gen.map(Some(_))))

  val assetPairGen = Gen.zip(assetIdGen, assetIdGen).suchThat(p => p._1 != p._2).map(p => AssetPair(p._1.map(ByteStr(_)), p._2.map(ByteStr(_))))

  val maxTimeGen: Gen[Long]     = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
  val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(NTP.correctedTime() - _)

  val config = loadConfig(ConfigFactory.parseString("""waves {
      |  directory: "/tmp/waves-test"
      |  matcher {
      |    enable: yes
      |    account: ""
      |    bind-address: "127.0.0.1"
      |    port: 6886
      |    order-history-file: null
      |    min-order-fee: 100000
      |    order-match-tx-fee: 100000
      |    snapshots-interval: 1d
      |    max-open-orders: 1000
      |    price-assets: ["BASE1", "BASE2", "BASE"]
      |    blacklisted-assets: ["BLACKLST"]
      |    blacklisted-names: ["[F,f]orbidden"]
      |  }
      |}""".stripMargin))

  val matcherSettings = MatcherSettings.fromConfig(config)

  def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) {
      value = gen.sample
    }
    value.get
  }

  val maxWavesAmountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  def buyGenerator(pair: AssetPair,
                   price: Long,
                   amount: Long,
                   sender: Option[PrivateKeyAccount] = None,
                   matcherFee: Option[Long] = None,
                   timestamp: Option[Long]): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long           <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def sellGenerator(pair: AssetPair,
                    price: Long,
                    amount: Long,
                    sender: Option[PrivateKeyAccount] = None,
                    matcherFee: Option[Long] = None,
                    timestamp: Option[Long]): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long           <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def buy(pair: AssetPair,
          price: BigDecimal,
          amount: Long,
          sender: Option[PrivateKeyAccount] = None,
          matcherFee: Option[Long] = None,
          ts: Option[Long] = None): Order = rawBuy(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee, ts)

  def rawBuy(pair: AssetPair,
             price: Price,
             amount: Long,
             sender: Option[PrivateKeyAccount] = None,
             matcherFee: Option[Long] = None,
             ts: Option[Long] = None): Order =
    valueFromGen(buyGenerator(pair, price, amount, sender, matcherFee, ts))._1

  def sell(pair: AssetPair,
           price: BigDecimal,
           amount: Long,
           sender: Option[PrivateKeyAccount] = None,
           matcherFee: Option[Long] = None,
           ts: Option[Long] = None): Order = rawSell(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee, ts)

  def rawSell(pair: AssetPair,
              price: Long,
              amount: Long,
              sender: Option[PrivateKeyAccount] = None,
              matcherFee: Option[Long] = None,
              ts: Option[Long] = None): Order =
    valueFromGen(sellGenerator(pair, price, amount, sender, matcherFee, ts))._1

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    orderType                 <- orderTypeGenerator
    price: Long               <- maxWavesAmountGen
    amount: Long              <- maxWavesAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxWavesAmountGen
    orderVersion: Byte        <- Gen.oneOf(1: Byte, 2: Byte)
  } yield (Order(sender, MatcherAccount, pair, orderType, price, amount, timestamp, expiration, matcherFee, orderVersion), sender)

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    price: Long               <- maxWavesAmountGen
    amount: Long              <- maxWavesAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxWavesAmountGen
    orderVersion: Byte        <- Gen.oneOf(1: Byte, 2: Byte)
  } yield
    BuyLimitOrder(price, amount, matcherFee, Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee, orderVersion))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    price: Long               <- maxWavesAmountGen
    amount: Long              <- maxWavesAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxWavesAmountGen
    orderVersion: Byte        <- Gen.oneOf(1: Byte, 2: Byte)
  } yield
    SellLimitOrder(price,
                   amount,
                   matcherFee,
                   Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee, orderVersion))

}
