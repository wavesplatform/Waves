package com.wavesplatform.state.diffs.smart

import com.wavesplatform.TestTime
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, SynchronousCalls}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.EitherValues

class VerifierComplexityLimitTest extends PropSpec with WithDomain with EitherValues {
  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val verifier = TestCompiler(V5).compileExpression {
    s"""
       | func f0() = true
       | ${(0 until 12).map(i => s"func f${i + 1}() = if (f$i()) then f$i() else f$i()").mkString("\n")}
       | ${(12 until 65).map(i => s"func f${i + 1}() = if (f$i()) then throw() else f$i()").mkString("\n")}
       | f65()
     """.stripMargin
  }

  private def features(fix: Boolean) =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(estimatorSumOverflowFixHeight = if (fix) 2 else 999)

  property("account verifier evaluation should be limited after RideV6 activation") {
    val account1  = accountGen.sample.get
    val account2  = accountGen.sample.get
    val fee       = ciFee().sample.get
    val genesis   = GenesisTransaction.create(account1.toAddress, ENOUGH_AMT, ts).explicitGet()
    val setScript = SetScriptTransaction.selfSigned(1.toByte, account1, Some(verifier), fee, ts).explicitGet()
    val checkTx   = () => TransferTransaction.selfSigned(2.toByte, account1, account2.toAddress, Waves, 1, Waves, fee, ByteStr.empty, ts).explicitGet()

    withDomain(domainSettingsWithFS(features(fix = false))) { d =>
      d.appendBlock(genesis, setScript)
      d.appendBlockE(checkTx()) should produce("Explicit script termination")
    }
    withDomain(domainSettingsWithFS(features(fix = true))) { d =>
      d.appendBlock(genesis, setScript)
      d.appendBlockE(checkTx()) should produce("Verifier complexity limit = 2000 is exceeded")
    }
  }

  property("asset verifier evaluation should be limited after RideV6 activation") {
    val account1  = accountGen.sample.get
    val account2  = accountGen.sample.get
    val fee       = ciFee().sample.get
    val genesis   = GenesisTransaction.create(account1.toAddress, ENOUGH_AMT, ts).explicitGet()
    val setScript = IssueTransaction.selfSigned(2.toByte, account1, "name", "", 1, 0, true, Some(verifier), fee, ts).explicitGet()
    val asset     = IssuedAsset(setScript.id())
    val checkTx   = () => TransferTransaction.selfSigned(2.toByte, account1, account2.toAddress, asset, 1, Waves, fee, ByteStr.empty, ts).explicitGet()

    withDomain(domainSettingsWithFS(features(fix = false))) { d =>
      d.appendBlock(genesis, setScript)
      d.appendBlockE(checkTx()) should produce("Explicit script termination")
    }
    withDomain(domainSettingsWithFS(features(fix = true))) { d =>
      d.appendBlock(genesis, setScript)
      d.appendBlockE(checkTx()) should produce("Verifier complexity limit = 4000 is exceeded")
    }
  }
}
