package com.wavesplatform.state.diffs.smart.performance

import com.wavesplatform.lang.v1.Terms.Typed
import com.wavesplatform.lang.v1.{Parser, TypeChecker}
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.state._
import com.wavesplatform.utils._
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.{NoShrink, OldTransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{TransferTransaction, VersionedTransferTransaction}
import scorex.transaction.smart.script.v1.ScriptV1

class SigVerifyPerformanceTest extends PropSpec with PropertyChecks with Matchers with OldTransactionGen with NoShrink with WithDB {

  private val AmtOfTxs = 10000

  private def simpleSendGen(from: PrivateKeyAccount, to: PublicKeyAccount, ts: Long): Gen[TransferTransaction] =
    for {
      amt <- smallFeeGen
      fee <- smallFeeGen
    } yield TransferTransaction.create(None, from, to.toAddress, amt, ts, None, fee, Array.emptyByteArray).explicitGet()

  private def scriptedSendGen(from: PrivateKeyAccount, to: PublicKeyAccount, ts: Long): Gen[VersionedTransferTransaction] =
    for {
      version <- Gen.oneOf(VersionedTransferTransaction.supportedVersions.toSeq)
      amt     <- smallFeeGen
      fee     <- smallFeeGen
    } yield VersionedTransferTransaction.selfSigned(version, None, from, to.toAddress, amt, ts, fee, Array.emptyByteArray).explicitGet()

  private def differentTransfers(typed: Typed.EXPR) =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      amt       <- smallFeeGen
      fee       <- smallFeeGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typed).explicitGet())
      transfer       = simpleSendGen(master, recipient, ts)
      scriptTransfer = scriptedSendGen(master, recipient, ts)
      transfers       <- Gen.listOfN(AmtOfTxs, transfer)
      scriptTransfers <- Gen.listOfN(AmtOfTxs, scriptTransfer)
    } yield (genesis, setScript, transfers, scriptTransfers)

  ignore("parallel native signature verification vs sequential scripted signature verification") {
    val textScript    = "sigVerify(tx.bodyBytes,tx.proof0,tx.senderPk)"
    val untypedScript = Parser(textScript).get.value
    val typedScript   = TypeChecker(dummyTypeCheckerContext, untypedScript).explicitGet()

    forAll(differentTransfers(typedScript)) {
      case (gen, setScript, transfers, scriptTransfers) =>
        def simpleCheck(): Unit = assertDiffAndState(Seq(TestBlock.create(Seq(gen))), TestBlock.create(transfers), smartEnabledFS) { case _ => }
        def scriptedCheck(): Unit =
          assertDiffAndState(Seq(TestBlock.create(Seq(gen, setScript))), TestBlock.create(scriptTransfers), smartEnabledFS) {
            case _ =>
          }

        val simeplCheckTime   = Instrumented.withTime(simpleCheck())._2
        val scriptedCheckTime = Instrumented.withTime(scriptedCheck())._2
        println(s"[parallel] simple check time: $simeplCheckTime ms,\t [seqential] scripted check time: $scriptedCheckTime ms")
    }

  }
}
