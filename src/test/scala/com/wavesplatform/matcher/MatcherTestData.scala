package com.wavesplatform.matcher

import java.util.concurrent.atomic.AtomicLong

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.matcher.model.{BuyLimitOrder, OrderValidator, SellLimitOrder}
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.settings.OrderFeeSettings.{FixedSettings, FixedWavesSettings, OrderFeeSettings, PercentSettings}
import com.wavesplatform.settings.{AssetType, loadConfig}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV3}
import com.wavesplatform.{NTPTime, crypto}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

import scala.util.Random

trait MatcherTestData extends NTPTime { _: Suite =>
  private val signatureSize = 32

  val bytes32gen: Gen[Array[Byte]]       = Gen.listOfN(signatureSize, Arbitrary.arbitrary[Byte]).map(xs => xs.toArray)
  val WalletSeed                         = ByteStr("Matcher".getBytes())
  val MatcherSeed: Array[Byte]           = crypto.secureHash(Bytes.concat(Ints.toByteArray(0), WalletSeed.arr))
  val MatcherAccount                     = PrivateKeyAccount(MatcherSeed)
  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long]         = Gen.choose(1, Long.MaxValue)

  private val seqNr = new AtomicLong(-1)

  def wrap(x: Order): QueueEventWithMeta                           = wrap(seqNr.incrementAndGet(), x)
  def wrap(n: Long, x: Order): QueueEventWithMeta                  = wrap(n, QueueEvent.Placed(x))
  private def wrap(n: Long, event: QueueEvent): QueueEventWithMeta = QueueEventWithMeta(n, System.currentTimeMillis(), event)

  def assetIdGen(prefix: Byte): Gen[IssuedAsset] =
    Gen
      .listOfN(signatureSize - 1, Arbitrary.arbitrary[Byte])
      .map(xs => IssuedAsset(ByteStr(Array(prefix, xs: _*))))

  def arbitraryAssetIdGen: Gen[IssuedAsset] = assetIdGen(Random.nextInt(Byte.MaxValue).toByte)

  val distinctPairGen: Gen[AssetPair] = for {
    a1 <- assetIdGen(1.toByte)
    a2 <- assetIdGen(2.toByte)
  } yield AssetPair(a1, a2)

  protected def mkAssetId(prefix: String): IssuedAsset = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    IssuedAsset(ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32)))
  }

  val assetPairGen = Gen.frequency((18, distinctPairGen), (1, assetIdGen(1).map(AssetPair(_, Waves))), (1, assetIdGen(2).map(AssetPair(Waves, _))))

  val maxTimeGen: Gen[Long]     = Gen.choose(10000L, Order.MaxLiveTime).map(_ + System.currentTimeMillis())
  val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(System.currentTimeMillis() - _)

  val config = loadConfig(ConfigFactory.parseString("""waves {
      |  directory: "/tmp/waves-test"
      |  matcher {
      |    enable: yes
      |    account: ""
      |    bind-address: "127.0.0.1"
      |    port: 6886
      |    order-history-file: null
      |    min-order-fee: 100000
      |    snapshots-interval: 100000
      |    max-open-orders: 1000
      |    price-assets: ["BASE1", "BASE2", "BASE"]
      |    blacklisted-assets: ["BLACKLST"]
      |    blacklisted-names: ["[Ff]orbidden"]
      |    allow-order-v3 = yes
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
                   amount: Long,
                   price: Long,
                   sender: Option[PrivateKeyAccount] = None,
                   matcherFee: Option[Long] = None,
                   version: Byte = 1,
                   timestamp: Option[Long]): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long           <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version), sender)

  def sellGenerator(pair: AssetPair,
                    amount: Price,
                    price: Price,
                    sender: Option[PrivateKeyAccount] = None,
                    matcherFee: Option[Price] = None,
                    timestamp: Option[Price],
                    version: Byte = 1): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long           <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version), sender)

  def buy(pair: AssetPair,
          amount: Price,
          price: BigDecimal,
          sender: Option[PrivateKeyAccount] = None,
          matcherFee: Option[Price] = None,
          ts: Option[Price] = None,
          version: Byte = 1): Order = rawBuy(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version)

  def rawBuy(pair: AssetPair,
             amount: Price,
             price: Price,
             sender: Option[PrivateKeyAccount] = None,
             matcherFee: Option[Price] = None,
             ts: Option[Price] = None,
             version: Byte = 1): Order =
    valueFromGen(buyGenerator(pair, amount, price, sender, matcherFee, version, ts))._1

  def sell(pair: AssetPair,
           amount: Price,
           price: BigDecimal,
           sender: Option[PrivateKeyAccount] = None,
           matcherFee: Option[Price] = None,
           ts: Option[Price] = None,
           version: Byte = 1): Order = rawSell(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version)

  def rawSell(pair: AssetPair,
              amount: Price,
              price: Price,
              sender: Option[PrivateKeyAccount] = None,
              matcherFee: Option[Price] = None,
              ts: Option[Price] = None,
              version: Byte = 1): Order =
    valueFromGen(sellGenerator(pair, amount, price, sender, matcherFee, ts, version))._1

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  def orderGenerator(sender: PrivateKeyAccount, pair: AssetPair): Gen[Order] =
    for {
      orderType          <- orderTypeGenerator
      amount: Long       <- maxWavesAmountGen
      price: Long        <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long    <- createdTimeGen
      expiration: Long   <- maxTimeGen
      matcherFee: Long   <- maxWavesAmountGen
      orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
      arbitraryAsset     <- arbitraryAssetIdGen
      matcherFeeAssetId  <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield {
      if (orderVersion == 3)
        Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion, matcherFeeAssetId)
      else Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion)
    }

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    order                     <- orderGenerator(sender, pair)
  } yield order -> sender

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    amount: Long              <- maxWavesAmountGen
    price: Long               <- maxWavesAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxWavesAmountGen
    orderVersion: Byte        <- Gen.oneOf(1: Byte, 2: Byte)
  } yield BuyLimitOrder(amount, matcherFee, Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    amount: Long              <- maxWavesAmountGen
    price: Long               <- maxWavesAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxWavesAmountGen
    orderVersion: Byte        <- Gen.oneOf(1: Byte, 2: Byte)
  } yield SellLimitOrder(amount, matcherFee, Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion))

  val orderV3Generator: Gen[Order] =
    for {
      sender: PrivateKeyAccount <- accountGen
      pair                      <- assetPairGen
      orderType                 <- orderTypeGenerator
      amount: Long              <- maxWavesAmountGen
      price: Long               <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long           <- createdTimeGen
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- maxWavesAmountGen
      arbitraryAsset            <- arbitraryAssetIdGen
      matcherFeeAssetId         <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield {
      OrderV3(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
    }

  def orderV3WithPredefinedFeeAssetGenerator(matcherFeeAsset: Option[Asset] = None): Gen[(PrivateKeyAccount, Order)] = {
    for {
      sender: PrivateKeyAccount <- accountGen
      pair                      <- distinctPairGen
      orderType                 <- orderTypeGenerator
      amount: Long              <- maxWavesAmountGen
      price: Long               <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long           <- createdTimeGen
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- maxWavesAmountGen
      arbitraryAsset            <- arbitraryAssetIdGen
    } yield {

      sender -> OrderV3(sender,
                        MatcherAccount,
                        pair,
                        orderType,
                        amount,
                        price,
                        timestamp,
                        expiration,
                        matcherFee,
                        matcherFeeAsset getOrElse arbitraryAsset)
    }
  }

  val orderV3PairGenerator: Gen[((PrivateKeyAccount, Order), (PrivateKeyAccount, Order))] =
    for {
      senderBuy: PrivateKeyAccount  <- accountGen
      senderSell: PrivateKeyAccount <- accountGen
      pair                          <- assetPairGen
      amount: Long                  <- maxWavesAmountGen
      price: Long                   <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestampBuy: Long            <- createdTimeGen
      timestampSell: Long           <- createdTimeGen
      expirationBuy: Long           <- maxTimeGen
      expirationSell: Long          <- maxTimeGen
      matcherFeeBuy: Long           <- maxWavesAmountGen
      matcherFeeSell: Long          <- maxWavesAmountGen
      arbitraryAsset                <- arbitraryAssetIdGen
      matcherFeeAssetId             <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield {
      (
        senderBuy -> OrderV3(senderBuy,
                             MatcherAccount,
                             pair,
                             OrderType.BUY,
                             Order.correctAmount(amount, price),
                             price,
                             timestampBuy,
                             expirationBuy,
                             matcherFeeBuy,
                             matcherFeeAssetId),
        senderSell -> OrderV3(senderSell,
                              MatcherAccount,
                              pair,
                              OrderType.SELL,
                              Order.correctAmount(amount, price),
                              price,
                              timestampSell,
                              expirationSell,
                              matcherFeeSell,
                              matcherFeeAssetId)
      )
    }

  val percentSettingsGenerator: Gen[PercentSettings] =
    for {
      assetType <- Gen.oneOf(AssetType.values.toSeq)
      minFee    <- Gen.choose(0.01, 100.0)
    } yield PercentSettings(assetType, minFee)

  def fixedSettingsGenerator(defaultAsset: Asset, lowerMinFeeBound: Long = 1, upperMinFeeBound: Long = 1000000L): Gen[FixedSettings] =
    for { minFee <- Gen.choose(lowerMinFeeBound, upperMinFeeBound) } yield { FixedSettings(defaultAsset, minFee) }

  def fixedWavesSettingsGenerator(lowerBaseFeeBound: Long = 1, upperBaseFeeBound: Long = 1000000L): Gen[FixedWavesSettings] =
    for { baseFee <- Gen.choose(lowerBaseFeeBound, upperBaseFeeBound) } yield { FixedWavesSettings(baseFee) }

  def orderFeeSettingsGenerator(defaultAssetForFixedSettings: Option[Asset] = None): Gen[OrderFeeSettings] = {
    for {
      defaultAsset     <- defaultAssetForFixedSettings.fold(arbitraryAssetIdGen)(_.fold(arbitraryAssetIdGen)(Gen.const))
      orderFeeSettings <- Gen.oneOf(fixedWavesSettingsGenerator(), fixedSettingsGenerator(defaultAsset), percentSettingsGenerator)
    } yield orderFeeSettings
  }

  val orderWithFeeSettingsGenerator: Gen[(Order, PrivateKeyAccount, OrderFeeSettings)] = {
    for {
      orderFeeSettings <- orderFeeSettingsGenerator()
      (order, sender)  <- orderGenerator
    } yield {
      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)
      (correctedOrder, sender, orderFeeSettings)
    }
  }

  def orderWithFeeSettingsGenerator(tpe: OrderType, price: Long): Gen[(Order, OrderFeeSettings)] =
    for {
      sender: PrivateKeyAccount <- accountGen
      pair                      <- assetPairGen
      amount: Long              <- maxWavesAmountGen
      timestamp: Long           <- createdTimeGen
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- maxWavesAmountGen
      orderVersion: Byte        <- Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
      arbitraryAsset            <- arbitraryAssetIdGen
      matcherFeeAssetId         <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
      orderFeeSettings          <- orderFeeSettingsGenerator(Some(arbitraryAsset))
    } yield {

      val order =
        if (orderVersion == 3)
          Order(sender, MatcherAccount, pair, tpe, amount, price, timestamp, expiration, matcherFee, orderVersion, matcherFeeAssetId)
        else Order(sender, MatcherAccount, pair, tpe, amount, price, timestamp, expiration, matcherFee, orderVersion)

      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)

      correctedOrder -> orderFeeSettings
    }

  val orderWithoutWavesInPairAndWithFeeSettingsGenerator: Gen[(Order, PrivateKeyAccount, OrderFeeSettings)] = {
    for {
      sender: PrivateKeyAccount <- accountGen
      amountAsset               <- arbitraryAssetIdGen
      priceAsset                <- arbitraryAssetIdGen
      orderFeeSettings          <- orderFeeSettingsGenerator()
      order                     <- orderGenerator(sender, AssetPair(amountAsset, priceAsset))
    } yield {
      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)
      (correctedOrder, sender, orderFeeSettings)
    }
  }

  val orderV3WithFeeSettingsGenerator: Gen[(Order, OrderFeeSettings)] = {
    for {
      (sender, order)  <- orderV3WithPredefinedFeeAssetGenerator()
      orderFeeSettings <- orderFeeSettingsGenerator(Some(order.matcherFeeAssetId))
    } yield {
      correctOrderByFeeSettings(order, sender, orderFeeSettings) -> orderFeeSettings
    }
  }

  private def correctOrderByFeeSettings(order: Order, sender: PrivateKeyAccount, orderFeeSettings: OrderFeeSettings): Order = {
    val correctedOrder = (order.version, orderFeeSettings) match {
      case (3, FixedSettings(defaultAssetId, minFee)) =>
        order
          .updateMatcherFeeAssetId(defaultAssetId)
          .updateFee(minFee)
      case (3, percentSettings: PercentSettings) =>
        order
          .updateMatcherFeeAssetId(OrderValidator.getValidFeeAssetForSettings(order, percentSettings))
          .updateFee(OrderValidator.getMinValidFeeForSettings(order, percentSettings, order.price))
      case (_, FixedWavesSettings(baseFee)) =>
        order
          .updateMatcherFeeAssetId(Waves)
          .updateFee(baseFee)
      case _ => order
    }

    Order.sign(correctedOrder, sender)
  }
}
