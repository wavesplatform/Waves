package com.wavesplatform.state2

import java.util.concurrent.TimeUnit

import com.wavesplatform.state2.StateSyntheticBenchmark.St
import org.openjdk.jmh.annotations._
import org.scalacheck.Gen
import scorex.account.PrivateKeyAccount
import scorex.transaction.Transaction
import scorex.transaction.assets.TransferTransaction

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class StateSyntheticBenchmark {

  @Benchmark
  def appendBlock_test(db: St): Unit = db.genAndApplyNextBlock()

}

object StateSyntheticBenchmark {

  @State(Scope.Thread)
  class St extends BaseState {
    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        amount    <- Gen.choose(1, waves(1))
        recipient <- accountGen
      } yield TransferTransaction.create(None, sender, recipient, amount, ts, None, 100000, Array.emptyByteArray).right.get
  }

}
