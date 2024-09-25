package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.{invoke, secondSigner, setScript}

class ScriptTransferByAliasTest extends PropSpec with WithDomain {

  private val activationHeight = 4

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> activationHeight
    ),
    estimatorPreCheckHeight = Int.MaxValue
  )

  private val verifier: Script =
    TestCompiler(V4).compileExpression(
      s"""
         |  {-# STDLIB_VERSION 4          #-}
         |  {-# CONTENT_TYPE   EXPRESSION #-}
         |
         |  match tx {
         |    case t: TransferTransaction =>
         |        match t.recipient {
         |            case alias: Alias     => true
         |            case address: Address => throw("alias expected!")
         |        }
         |    case _ => true
         |  }
       """.stripMargin
    )

  private val transferAmount = 123
  private val alias          = "alias"

  private def dApp(asset: IssuedAsset): Script = {
    TestCompiler(V4).compileContract(
      s"""
         | {-# STDLIB_VERSION 4       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() =
         |   [
         |     ScriptTransfer(Alias("$alias"), $transferAmount, base58'$asset')
         |   ]
       """.stripMargin
    )
  }

  property(s"ScriptTransfer alias recipient is mapped correctly after ${BlockchainFeatures.SynchronousCalls} activation") {
    val dAppAcc  = TxHelpers.signer(0)
    val invoker  = TxHelpers.signer(1)
    val receiver = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(dAppAcc, invoker, receiver)

    val createAlias  = TxHelpers.createAlias(alias, receiver)
    val issue        = TxHelpers.issue(dAppAcc, ENOUGH_AMT, script = Some(verifier))
    val asset        = IssuedAsset(issue.id())
    val setDApp      = TxHelpers.setScript(dAppAcc, dApp(asset))
    val preparingTxs = Seq(createAlias, issue, setDApp)

    def invoke = TxHelpers.invoke(dAppAcc.toAddress, func = None, invoker = invoker, fee = TxHelpers.ciFee(sc = 1))

    withDomain(domainSettingsWithFS(fsWithV5), balances) { d =>
      d.appendBlock(preparingTxs*)

      d.appendAndAssertFailed(invoke, s"Transaction is not allowed by script of the asset $asset: alias expected!")

      d.appendBlock()
      d.blockchainUpdater.height shouldBe activationHeight

      d.appendAndAssertSucceed(invoke)
      d.balance(receiver.toAddress, asset) shouldBe transferAmount
    }
  }

  property("unexisting ScriptTransfer alias recipient") {
    withDomain(DomainPresets.RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = [
          |   ScriptTransfer(Alias("alias"), 1, unit)
          | ]
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlockE(invoke()) should produce("Alias 'alias:T:alias' does not exist")
    }
  }
}
