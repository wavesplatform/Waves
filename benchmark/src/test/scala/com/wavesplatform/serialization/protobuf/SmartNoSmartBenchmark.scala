package com.wavesplatform.serialization.protobuf

import java.util.concurrent.TimeUnit

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, _}
import com.wavesplatform.serialization.protobuf.SmartNoSmartBenchmark.ExchangeTransactionSt
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

//noinspection ScalaStyle
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class SmartNoSmartBenchmark {
  @Benchmark
  def smartExchangeTX_test(st: ExchangeTransactionSt, bh: Blackhole): Unit = {
    import st._
    val exchangeTransaction = ExchangeTransactionV2.create(
      buy,
      sell,
      2,
      5000000000L,
      1,
      1,
      1,
      1526992336241L,
      proofs
    )

    bh.consume(exchangeTransaction.right.get)
  }

  @Benchmark
  def unsafeExchangeTX_test(st: ExchangeTransactionSt, bh: Blackhole): Unit = {
    import st._
    val exchangeTransaction = ExchangeTransactionV2(
      buy,
      sell,
      2,
      5000000000L,
      1,
      1,
      1,
      1526992336241L,
      proofs
    )

    bh.consume(exchangeTransaction)
  }
}

object SmartNoSmartBenchmark {
  @State(Scope.Benchmark)
  class ExchangeTransactionSt {
    val buy = OrderV2(
      PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.BUY,
      2,
      6000000000L,
      1526992336241L,
      1529584336241L,
      1,
      Proofs(Seq(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get))
    )

    val sell = OrderV1(
      PublicKey.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").explicitGet(),
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      3,
      5000000000L,
      1526992336241L,
      1529584336241L,
      2,
      Base58.tryDecodeWithLimit("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get
    )

    val proofs = Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))
  }
}
