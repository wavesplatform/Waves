package com.wavesplatform.serialization.protobuf

import java.util.concurrent.TimeUnit

import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, _}
import com.wavesplatform.protobuf.transaction.{PBCachedTransaction, PBTransactions}
import com.wavesplatform.serialization.protobuf.ProtoBufBenchmark.ExchangeTransactionSt
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.{FastHashId, Proofs}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ProtoBufBenchmark {

  @Benchmark
  def serializeMassTransferPB_test(bh: Blackhole): Unit = {
    val vanillaTx = {
      val transfers = MassTransferTransaction
        .parseTransfersList(
          List(Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 100000000L), Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 200000000L)))
        .right
        .get

      MassTransferTransaction
        .create(
          Waves,
          PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
          transfers,
          1518091313964L,
          200000,
          Base58.decode("59QuUcqP6p").get,
          Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
        )
        .right
        .get
    }

    val tx = PBTransactions.protobuf(vanillaTx)
    bh.consume(tx.toByteArray)
  }

  @Benchmark
  def serializeMassTransferVanilla_test(bh: Blackhole): Unit = {
    val vanillaTx = {
      val transfers = MassTransferTransaction
        .parseTransfersList(
          List(Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 100000000L), Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 200000000L)))
        .right
        .get

      MassTransferTransaction
        .create(
          Waves,
          PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
          transfers,
          1518091313964L,
          200000,
          Base58.decode("59QuUcqP6p").get,
          Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
        )
        .right
        .get
    }

    bh.consume(vanillaTx.bytes())
  }

  @Benchmark
  def readCachedTx_test(st: ExchangeTransactionSt, bh: Blackhole): Unit = {
    val cached = PBCachedTransaction(st.exchangeTransaction)
    bh.consume(cached.id)
    bh.consume(cached.bytes)
    bh.consume(cached.withProofs(st.exchangeTransaction.proofs.reverse: _*).bytes)
  }

  @Benchmark
  def readUncachedTx_test(st: ExchangeTransactionSt, bh: Blackhole): Unit = {
    bh.consume(st.exchangeTransaction.toByteArray)
    bh.consume(FastHashId.create(st.exchangeTransaction.getTransaction.toByteArray))
    bh.consume(st.exchangeTransaction.withProofs(st.exchangeTransaction.proofs.reverse).toByteArray)
  }
}

object ProtoBufBenchmark {
  @State(Scope.Benchmark)
  class ExchangeTransactionSt {
    val buy = OrderV2(
      PublicKeyAccount.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
      PublicKeyAccount.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
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
      PublicKeyAccount.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").explicitGet(),
      PublicKeyAccount.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      3,
      5000000000L,
      1526992336241L,
      1529584336241L,
      2,
      Base58.decode("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get
    )

    val proofs = Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))

    val exchangeTransaction = PBTransactions.protobuf(
      ExchangeTransactionV2(
        buy,
        sell,
        2,
        5000000000L,
        1,
        1,
        1,
        1526992336241L,
        proofs
      ))
  }
}
