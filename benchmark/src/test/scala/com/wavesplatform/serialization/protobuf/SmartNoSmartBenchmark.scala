package com.wavesplatform.serialization.protobuf

import java.util.concurrent.TimeUnit

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.serialization.protobuf.SmartNoSmartBenchmark.ExchangeTransactionSt
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.{Proofs, TxExchangeAmount, TxExchangePrice, TxMatcherFee, TxOrderPrice, TxPositiveAmount, TxVersion}
import org.openjdk.jmh.annotations.*
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
    import st.*
    val exchangeTransaction = ExchangeTransaction.create(TxVersion.V2, buy, sell, 2, 5000000000L, 1, 1, 1, 1526992336241L, proofs)
    bh.consume(exchangeTransaction.explicitGet())
  }

  @Benchmark
  def unsafeExchangeTX_test(st: ExchangeTransactionSt, bh: Blackhole): Unit = {
    import st.*
    val exchangeTransaction = ExchangeTransaction(
      TxVersion.V2,
      buy,
      sell,
      TxExchangeAmount.unsafeFrom(2),
      TxExchangePrice.unsafeFrom(5000000000L),
      1,
      1,
      TxPositiveAmount.unsafeFrom(1),
      1526992336241L,
      proofs,
      AddressScheme.current.chainId
    )
    bh.consume(exchangeTransaction)
  }
}

object SmartNoSmartBenchmark {
  @State(Scope.Benchmark)
  class ExchangeTransactionSt {
    val buy = Order(
      TxVersion.V2,
      OrderAuthentication.OrderProofs(
        PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
        Proofs(Seq(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get))
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
      TxVersion.V1,
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

    val proofs = Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))
  }
}
