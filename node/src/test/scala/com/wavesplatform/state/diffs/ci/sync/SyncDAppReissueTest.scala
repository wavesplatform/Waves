package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, SynchronousCalls}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, TxHelpers}

class SyncDAppReissueTest extends PropSpec with WithDomain {

  property("allow transfer after reissue enough funds") {
    val invoker   = TxHelpers.signer(0)
    val dApp      = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val assetAmount = 100

    val balances = AddrWithBalance.enoughBalances(invoker, dApp)

    val issue     = TxHelpers.issue(dApp, assetAmount - 1)
    val asset     = IssuedAsset(issue.id.value())
    val setScript = TxHelpers.setScript(dApp, transferAfterReissueDAppScript(recipient.toAddress, asset, assetAmount))

    val invoke = TxHelpers.invoke(dApp.toAddress, invoker = invoker)

    withDomain(RideV5, balances) { d =>
      d.appendBlock(issue, setScript)

      d.appendAndAssertSucceed(invoke)
      d.accountsApi.assetBalance(recipient.toAddress, asset) shouldBe assetAmount
    }
  }

  property("negative reissue quantity") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

      val issue      = TxHelpers.issue(dApp2, 100)
      val asset      = IssuedAsset(issue.id.value())
      val setScript1 = TxHelpers.setScript(dApp1, invokeDAppScript(dApp2.toAddress, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, reissueDAppScript(asset, -1, bigComplexityDApp2))

      val preparingTxs = Seq(issue, setScript1, setScript2)

      val invoke1 = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker)
      val invoke2 = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker)

      withDomain(
        DomainPresets
          .domainSettingsWithFS(
            TestFunctionalitySettings
              .withFeatures(BlockV5, SynchronousCalls)
              .copy(enforceTransferValidationAfter = 4)
          ),
        balances
      ) { d =>
        d.appendBlock(preparingTxs*)

        d.appendBlock(invoke1)
        d.blockchain.transactionSucceeded(invoke1.txId) shouldBe true
        d.blockchain.balance(dApp2.toAddress, asset) shouldBe 99

        d.appendBlockE(invoke2) should produce("Negative reissue quantity = -1")
      }
    }
  }

  property("reissue foreign asset") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

      val issue      = TxHelpers.issue(dApp1, 100)
      val asset      = IssuedAsset(issue.id.value())
      val setScript1 = TxHelpers.setScript(dApp1, invokeDAppScript(dApp2.toAddress, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, reissueDAppScript(asset, 1, bigComplexityDApp2))

      val preparingTxs = Seq(issue, setScript1, setScript2)

      val invoke = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker)

      withDomain(RideV5.configure(_.copy(enforceTransferValidationAfter = 100)), balances) { d =>
        d.appendBlock(preparingTxs*)

        if (bigComplexityDApp1 || bigComplexityDApp2) {
          d.appendBlock(invoke)
          d.liquidDiff.errorMessage(invoke.txId).get.text should include("Asset was issued by other address")
        } else {
          d.appendBlockE(invoke) should produce("Asset was issued by other address")
        }
      }
    }
  }

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def invokeDAppScript(dApp2: Address, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke("default", [], [])
         |    []
         | }
       """.stripMargin
    )

  private def reissueDAppScript(asset: Asset, amount: Long, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   [
         |     Reissue(base58'$asset', $amount, true)
         |   ]
         | }
       """.stripMargin
    )

  private def transferAfterReissueDAppScript(recipient: Address, asset: Asset, amount: Long): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = this.invoke("reissue", [], [])
         |    [
         |      ScriptTransfer(Address(base58'$recipient'), $amount, base58'$asset')
         |    ]
         | }
         | 
         | @Callable(i)
         | func reissue() = {
         |   [Reissue(base58'$asset', 1, true)]
         | }
       """.stripMargin
    )
}
