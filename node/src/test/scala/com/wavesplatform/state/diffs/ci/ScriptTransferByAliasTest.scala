package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{CreateAliasTransaction, GenesisTransaction, Transaction}
import org.scalacheck.Gen

class ScriptTransferByAliasTest extends PropSpec with WithDomain {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val activationHeight = 3

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

  private val paymentPreconditions: Gen[(List[Transaction], () => InvokeScriptTransaction, IssuedAsset, Address)] =
    for {
      dAppAcc  <- accountGen
      invoker  <- accountGen
      receiver <- accountGen
      fee      <- ciFee(sc = 1)
    } yield {
      for {
        genesis     <- GenesisTransaction.create(dAppAcc.toAddress, ENOUGH_AMT, ts)
        genesis2    <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        genesis3    <- GenesisTransaction.create(receiver.toAddress, ENOUGH_AMT, ts)
        createAlias <- CreateAliasTransaction.selfSigned(2.toByte, receiver, alias, fee, ts)
        issue       <- IssueTransaction.selfSigned(2.toByte, dAppAcc, "Asset", "Description", ENOUGH_AMT, 8, true, Some(verifier), fee, ts)
        asset = IssuedAsset(issue.id())
        setDApp <- SetScriptTransaction.selfSigned(1.toByte, dAppAcc, Some(dApp(asset)), fee, ts)
        invoke = () => Signed.invokeScript(1.toByte, invoker, dAppAcc.toAddress, None, Nil, fee, Waves, ts)
      } yield (List(genesis, genesis2, genesis3, createAlias, issue, setDApp), invoke, asset, receiver.toAddress)
    }.explicitGet()

  property(s"ScriptTransfer alias recipient is mapped correctly after ${BlockchainFeatures.SynchronousCalls} activation") {
    val (preparingTxs, invoke, asset, receiver) = paymentPreconditions.sample.get
    withDomain(domainSettingsWithFS(fsWithV5)) { d =>
      d.appendBlock(preparingTxs*)

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.blockchain.bestLiquidDiff.get.errorMessage(invoke1.id()).get.text should include(
        s"Transaction is not allowed by script of the asset $asset: alias expected!"
      )

      d.appendBlock()
      d.blockchainUpdater.height shouldBe activationHeight

      val invoke2 = invoke()
      d.appendBlock(invoke2)
      d.blockchain.bestLiquidDiff.get.errorMessage(invoke2.id()) shouldBe None
      d.balance(receiver, asset) shouldBe transferAmount
    }
  }
}
