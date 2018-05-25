package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.StateSyntheticBenchmark._
import com.wavesplatform.utils.dummyTypeCheckerContext
import org.openjdk.jmh.annotations._
import org.scalacheck.Gen
import scorex.account.PrivateKeyAccount
import scorex.transaction.Transaction
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class StateSyntheticBenchmark {

  @Benchmark
  def appendBlock_test(db: St): Unit = db.genAndApplyNextBlock()

  @Benchmark
  def appendBlock_smart_test(db: SmartSt): Unit = db.genAndApplyNextBlock()

}

object StateSyntheticBenchmark {

  @State(Scope.Benchmark)
  class St extends BaseState {
    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        amount    <- Gen.choose(1, waves(1))
        recipient <- accountGen
      } yield TransferTransactionV1.selfSigned(None, sender, recipient, amount, ts, None, 100000, Array.emptyByteArray).right.get
  }

  @State(Scope.Benchmark)
  class SmartSt extends BaseState {

    override protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = {
      base.copy(preActivatedFeatures = Map(4.toShort -> 0))
    }

    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        recipient: PrivateKeyAccount <- accountGen
        amount                       <- Gen.choose(1, waves(1))
      } yield
        TransferTransactionV2
          .selfSigned(
            TransferTransactionV2.supportedVersions.head,
            None,
            sender,
            recipient.toAddress,
            amount,
            ts,
            None,
            300000,
            Array.emptyByteArray
          )
          .explicitGet()

    @Setup
    override def init(): Unit = {
      super.init()

      val textScript    = "sigVerify(tx.bodyBytes,tx.proof0,tx.senderPk)"
      val untypedScript = Parser(textScript).get.value
      assert(untypedScript.size == 1)
      val typedScript = CompilerV1(dummyTypeCheckerContext, untypedScript.head).explicitGet()

      val setScriptBlock = nextBlock(
        Seq(
          SetScriptTransaction
            .selfSigned(
              SetScriptTransaction.supportedVersions.head,
              richAccount,
              Some(ScriptV1(typedScript).explicitGet()),
              100000,
              System.currentTimeMillis()
            )
            .explicitGet()
        )
      )

      applyBlock(setScriptBlock)
    }
  }

}
