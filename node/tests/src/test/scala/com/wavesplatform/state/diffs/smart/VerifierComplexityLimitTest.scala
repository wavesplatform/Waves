package com.wavesplatform.state.diffs.smart

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.EitherValues

class VerifierComplexityLimitTest extends PropSpec with WithDomain with EitherValues {

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
      .copy(estimatorSumOverflowFixHeight = if (fix) 3 else 999)

  property("account verifier evaluation should be limited after RideV6 activation") {
    val account1 = TxHelpers.signer(1)
    val account2 = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(account1)

    val setScript = TxHelpers.setScript(account1, verifier)
    val checkTx   = () => TxHelpers.transfer(account1, account2.toAddress, 1)

    withDomain(domainSettingsWithFS(features(fix = false)), balances) { d =>
      d.appendBlock(setScript)
      d.appendBlockE(checkTx()) should produce("Explicit script termination")
    }
    withDomain(domainSettingsWithFS(features(fix = true)), balances) { d =>
      d.appendBlock(setScript)
      d.appendBlockE(checkTx()) should produce("Verifier complexity limit = 2000 is exceeded")
    }
  }

  property("asset verifier evaluation should be limited after RideV6 activation") {
    val account1 = TxHelpers.signer(1)
    val account2 = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(account1)

    val issue   = TxHelpers.issue(account1, 1, script = Some(verifier))
    val asset   = IssuedAsset(issue.id())
    val checkTx = () => TxHelpers.transfer(account1, account2.toAddress, 1, asset)

    withDomain(domainSettingsWithFS(features(fix = false)), balances) { d =>
      d.appendBlock(issue)
      d.appendBlockE(checkTx()) should produce("Explicit script termination")
    }
    withDomain(domainSettingsWithFS(features(fix = true)), balances) { d =>
      d.appendBlock(issue)
      d.appendBlockE(checkTx()) should produce("Verifier complexity limit = 4000 is exceeded")
    }
  }
  property("verifier zero complexity test") {
    val feats = Seq(
      SmallerMinimalGeneratingBalance -> 0,
      NG                              -> 0,
      MassTransfer                    -> 0,
      SmartAccounts                   -> 0,
      DataTransaction                 -> 0,
      BurnAnyTokens                   -> 0,
      FeeSponsorship                  -> 0,
      FairPoS                         -> 0,
      SmartAssets                     -> 0,
      SmartAccountTrading             -> 0,
      Ride4DApps                      -> 0,
      BlockReward                     -> 0,
      BlockV5                         -> 0,
      SynchronousCalls                -> 0,
      ConsensusImprovements           -> 0,
      BlockRewardDistribution         -> 0,
      CappedReward                    -> 0,
      CeaseXtnBuyback                 -> 0,
      LightNode                       -> 0
    )

    val featsWithoutRideV6 = feats ++ Seq(RideV6 -> 100)
    val featsWithRideV6    = feats ++ Seq(RideV6 -> 0)

    val account1 = TxHelpers.signer(1)
    val account2 = TxHelpers.signer(2)
    val account3 = TxHelpers.signer(3)

    val balances = AddrWithBalance.enoughBalances(account1, account2, account3)

    val dAppScript = TestCompiler(V5).compileContract(
      s"""
         |  {-# STDLIB_VERSION 5 #-}
         |  {-# CONTENT_TYPE DAPP #-}
         |  {-# SCRIPT_TYPE ACCOUNT #-}
         |
         |  # empty, but has 1 complexity after RideV6 activation
         |  @Callable(i)
         |  func lightCall() = nil
         |
         |  @Verifier(tx)
         |  func verify() = true
    """.stripMargin
    )
    val accountScript = TestCompiler(V5).compileExpression(
      s"""
         | {-# STDLIB_VERSION 5 #-}
         | {-# CONTENT_TYPE EXPRESSION #-}
         | {-# SCRIPT_TYPE ACCOUNT #-}
         |
         | true
         |""".stripMargin
    )

    val setDAppScript    = TxHelpers.setScript(account1, dAppScript)
    val setAccountScript = TxHelpers.setScript(account2, accountScript)
    val checkVerifier    = from => TxHelpers.transfer(from, account3.toAddress, 1)

    val invoke =
      TxHelpers.invoke(
        account1.toAddress,
        invoker = account3,
        func = Some("lightCall"),
        args = Seq()
      )

    println("========Without RideV6=========")
    withDomain(domainSettingsWithFS(TestFunctionalitySettings.withFeaturesByHeight(featsWithoutRideV6*)), balances) { d =>
      d.appendBlock(setDAppScript, setAccountScript)
      d.appendBlockE(checkVerifier(account1)).isRight shouldBe true // dApp verifier
      d.appendBlockE(invoke).isRight shouldBe true
      d.appendBlockE(checkVerifier(account2)).isRight shouldBe true // account verifier
    }
    println("========With RideV6=========")
    withDomain(domainSettingsWithFS(TestFunctionalitySettings.withFeaturesByHeight(featsWithRideV6*)), balances) { d =>
      d.appendBlock(setDAppScript, setAccountScript)
      d.appendBlockE(checkVerifier(account1)).isRight shouldBe true // dApp verifier
      d.appendBlockE(invoke).isRight shouldBe true
      d.appendBlockE(checkVerifier(account2)).isRight shouldBe true // account verifier
    }
  }
}
