package com.wavesplatform.matcher

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.matcher.model.{BuyLimitOrder, LimitOrder, SellLimitOrder}
import org.scalacheck.{Arbitrary, Gen}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction._
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.{ByteArray, NTP}

trait MatcherTestData  {
  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val WalletSeed = "Matcher".getBytes
  val MatcherSeed =  SecureCryptographicHash(Bytes.concat(Ints.toByteArray(0), WalletSeed))
  val MatcherAccount = new PrivateKeyAccount(MatcherSeed)
  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => new PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  val maxWavesAnountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  val wavesAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]] = Gen.frequency((1, wavesAssetGen), (10, bytes32gen.map(Some(_))))

  val assetPairGen = Gen.zip(assetIdGen, assetIdGen).
    suchThat(p => !ByteArray.sameOption(p._1, p._2)).
    map(p => AssetPair(p._1, p._2))

  val maxTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())

  def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) {
      value = gen.sample
    }
    value.get
  }

  def buyGenerator(pair: AssetPair, price: Int, amount: Long): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- accountGen
      maxtTime: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAnountGen
    } yield (Order.buy(sender, MatcherAccount, pair, price, amount, maxtTime, matcherFee), sender)

  def sellGenerator(pair: AssetPair, price: Int, amount: Long): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- accountGen
      maxtTime: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAnountGen
    } yield (Order.sell(sender, MatcherAccount, pair, price, amount, maxtTime, matcherFee), sender)

  def buy(pair: AssetPair, price: Int, amount: Long): Order = valueFromGen(buyGenerator(pair, price, amount))._1

  def sell(pair: AssetPair, price: Int, amount: Long) = valueFromGen(sellGenerator(pair, price, amount))._1

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    price: Long <- maxWavesAnountGen
    amount: Long <- maxWavesAnountGen
    maxtTime: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAnountGen
  } yield (Order(sender, MatcherAccount, pair.first, pair.second, price, amount, maxtTime, matcherFee), sender)

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    price: Long <- maxWavesAnountGen
    amount: Long <- maxWavesAnountGen
    maxtTime: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAnountGen
  } yield BuyLimitOrder(price, amount, Order.buy(sender, MatcherAccount, pair, price, amount, maxtTime, matcherFee))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    price: Long <- maxWavesAnountGen
    amount: Long <- maxWavesAnountGen
    maxtTime: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAnountGen
  } yield SellLimitOrder(price, amount, Order.sell(sender, MatcherAccount, pair, price, amount, maxtTime, matcherFee))

}
