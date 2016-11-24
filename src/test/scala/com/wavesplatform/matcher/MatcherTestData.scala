package com.wavesplatform.matcher

import org.scalacheck.{Arbitrary, Gen}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction._
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.{ByteArray, NTP}

trait MatcherTestData  {
  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => new PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  val maxWavesAnountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  val wavesAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]] = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen)))

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
      matcher: PrivateKeyAccount <- accountGen
      maxtTime: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAnountGen
    } yield (Order.buy(sender, matcher, pair, price, amount, maxtTime, matcherFee), sender)

  def sellGenerator(pair: AssetPair, price: Int, amount: Long): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- accountGen
      matcher: PrivateKeyAccount <- accountGen
      maxtTime: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAnountGen
    } yield (Order.sell(sender, matcher, pair, price, amount, maxtTime, matcherFee), sender)

  def buy(pair: AssetPair, price: Int, amount: Long): Order = valueFromGen(buyGenerator(pair, price, amount))._1

  def sell(pair: AssetPair, price: Int, amount: Long) = valueFromGen(sellGenerator(pair, price, amount))._1

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    spendAssetID: Option[AssetId] <- assetIdGen
    receiveAssetID: Option[AssetId] <- assetIdGen
    price: Long <- maxWavesAnountGen
    amount: Long <- maxWavesAnountGen
    maxtTime: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAnountGen
  } yield (Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxtTime, matcherFee), sender)

}
