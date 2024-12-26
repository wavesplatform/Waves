package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, TxHelpers}

class SyncDAppBurnTest extends PropSpec with WithDomain {

  property("negative balance rejects or fails tx") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

      val assetAmount = 100

      val issue      = TxHelpers.issue(dApp2, assetAmount)
      val asset      = IssuedAsset(issue.id.value())
      val transfer   = TxHelpers.transfer(dApp2, dApp1.toAddress, assetAmount, asset)
      val setScript1 = TxHelpers.setScript(dApp1, invokeWithTransferDAppScript(dApp2.toAddress, asset, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, burnDAppScript(asset, assetAmount, bigComplexityDApp2))

      val preparingTxs = Seq(issue, transfer, setScript1, setScript2)

      val invoke = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker)

      withDomain(
        RideV5
          .configure(_.copy(enforceTransferValidationAfter = 3))
          .setFeaturesHeight(BlockchainFeatures.RideV6 -> 4),
        balances
      ) { d =>
        d.appendBlock(preparingTxs*)

        d.appendAndCatchError(invoke).toString should include("Negative asset")
        d.appendBlock()

        if (!bigComplexityDApp1 && !bigComplexityDApp2) {
          d.appendAndCatchError(invoke).toString should include("negative asset balance")
        } else {
          d.appendAndAssertFailed(invoke)
        }
      }
    }
  }

  property("can't burn transferred funds") {
    val invoker   = TxHelpers.signer(0)
    val dApp      = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val assetAmount = 100

    val balances = AddrWithBalance.enoughBalances(invoker, dApp)

    val issue     = TxHelpers.issue(dApp, assetAmount)
    val asset     = IssuedAsset(issue.id.value())
    val setScript = TxHelpers.setScript(dApp, burnAfterTransferDAppScript(recipient.toAddress, asset, assetAmount))

    val invoke = TxHelpers.invoke(dApp.toAddress, invoker = invoker)

    withDomain(RideV5, balances) { d =>
      d.appendBlock(issue, setScript)

      d.appendBlockE(invoke) should produce(
        s"${dApp.toAddress} -> negative asset balance: ${dApp.toAddress}, new portfolio: VectorMap(${asset.id} -> -$assetAmount"
      )
    }
  }

  property("can't transfer burned funds") {
    val invoker   = TxHelpers.signer(0)
    val dApp      = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val assetAmount = 100

    val balances = AddrWithBalance.enoughBalances(invoker, dApp)

    val issue     = TxHelpers.issue(dApp, assetAmount)
    val asset     = IssuedAsset(issue.id.value())
    val setScript = TxHelpers.setScript(dApp, transferAfterBurnDAppScript(recipient.toAddress, asset, assetAmount))

    val invoke = TxHelpers.invoke(dApp.toAddress, invoker = invoker)

    withDomain(RideV5, balances) { d =>
      d.appendBlock(issue, setScript)

      d.appendBlockE(invoke) should produce(
        s"Map(${dApp.toAddress} -> negative asset balance: ${dApp.toAddress}, new portfolio: VectorMap(${asset.id} -> -$assetAmount"
      )
    }
  }

  property("reissue and burn actions in sync call result state") {
    val invoker = TxHelpers.signer(1)
    val dApp    = TxHelpers.signer(2)

    val issueAmount   = 100
    val reissueAmount = 10
    val burnAmount    = 5

    val genesis   = Seq(invoker, dApp).map(acc => TxHelpers.genesis(acc.toAddress))
    val issue     = TxHelpers.issue(dApp, issueAmount)
    val setScript = TxHelpers.setScript(dApp, reissueAndBurnDAppScript(issue.asset, reissueAmount, burnAmount))
    val invoke    = TxHelpers.invoke(dApp.toAddress, invoker = invoker)

    assertDiffAndState(
      Seq(TestBlock.create(genesis :+ setScript :+ issue)),
      TestBlock.create(Seq(invoke)),
      RideV5.blockchainSettings.functionalitySettings
    ) { case (_, blockchain) =>
      val asset        = IssuedAsset(issue.id())
      val resultAmount = issue.quantity.value + reissueAmount - burnAmount

      blockchain.assetDescription(asset).get.totalVolume shouldBe resultAmount
      blockchain.balance(dApp.toAddress, asset) shouldBe resultAmount
    }
  }

  property("negative burn quantity") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {

      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

      val assetAmount = 100

      val issue      = TxHelpers.issue(dApp2, assetAmount)
      val asset      = IssuedAsset(issue.id.value())
      val setScript1 = TxHelpers.setScript(dApp1, invokeDAppScript(dApp2.toAddress, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, burnDAppScript(asset, -1, bigComplexityDApp2))

      val preparingTxs = Seq(issue, setScript1, setScript2)

      val invoke = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker)

      withDomain(
        RideV5
          .configure(_.copy(enforceTransferValidationAfter = 3))
          .setFeaturesHeight(BlockchainFeatures.RideV6 -> 4),
        balances
      ) { d =>
        d.appendBlock(preparingTxs*)

        d.appendAndCatchError(invoke).toString should include("Negative burn quantity")
        d.appendBlock()

        if (!bigComplexityDApp1 && !bigComplexityDApp2) {
          d.appendAndCatchError(invoke).toString should include("Negative burn quantity")
        } else {
          d.appendAndAssertFailed(invoke)
        }
      }
    }
  }

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def invokeWithTransferDAppScript(dApp2: Address, asset: Asset, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke("default", [], [])
         |    [
         |      ScriptTransfer(Address(base58'$dApp2'), 100, base58'$asset')
         |    ]
         | }
       """.stripMargin
    )

  private def reissueAndBurnDAppScript(asset: Asset, reissueAmount: Long, burnAmount: Long): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = this.invoke("reissueAndBurn", [], [])
         |    []
         | }
         |
         | @Callable(i)
         | func reissueAndBurn() = {
         |    [
         |      Reissue(base58'$asset', $reissueAmount, true),
         |      Burn(base58'$asset', $burnAmount)
         |    ]
         | }
       """.stripMargin
    )

  private def burnDAppScript(asset: Asset, amount: Long, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   [
         |     Burn(base58'$asset', $amount)
         |   ]
         | }
       """.stripMargin
    )

  private def burnAfterTransferDAppScript(recipient: Address, asset: Asset, amount: Long): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = this.invoke("transfer", [], [])
         |    [
         |      Burn(base58'$asset', $amount)
         |    ]
         | }
         | 
         | @Callable(i)
         | func transfer() = {
         |   [ScriptTransfer(Address(base58'$recipient'), $amount, base58'$asset')]
         | }
       """.stripMargin
    )

  private def transferAfterBurnDAppScript(recipient: Address, asset: Asset, amount: Long): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = this.invoke("burn", [], [])
         |    [
         |      ScriptTransfer(Address(base58'$recipient'), $amount, base58'$asset')
         |    ]
         | }
         | 
         | @Callable(i)
         | func burn() = {
         |   [Burn(base58'$asset', $amount)]
         | }
       """.stripMargin
    )

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
}
