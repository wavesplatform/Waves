package com.wavesplatform.state.diffs

import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.*
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxNonNegativeAmount, TxVersion}
import org.scalatest.Inside

// noinspection NameBooleanParameters
class ScriptComplexityCountTest extends PropSpec with WithDomain with Inside {
  val script = TestCompiler(V6).compileExpression("func f() = true\n f()")
  // complexity = 1 for both estimator and evaluator

  property("check scripts run count") {
    val (genesis, txs) = preconditions

    assertDiffAndState(
      Seq(TestBlock.create(Seq(genesis))),
      TestBlock.create(txs),
      RideV6.blockchainSettings.functionalitySettings
    ) { case (blockDiff, _) =>
      val scriptComplexity = Script
        .estimate(script, ScriptEstimatorV3(false, true), fixEstimateOfVerifier = true, useContractVerifierLimit = false)
        .explicitGet()
      blockDiff.scriptsComplexity shouldBe scriptComplexity * 31
    }
  }

  private def preconditions: (GenesisTransaction, Seq[Transaction]) = {
    val master = signer(1)
    val acc    = signer(2)

    val issueAmount      = 1000
    val additionalAmount = 1000000000L

    val genesisTx     = genesis(master.toAddress)
    val setContract   = setScript(master, script)
    val resetContract = setScript(master, script)
    val issueSp       = issue(master, issueAmount + additionalAmount)
    val sponsorTx     = sponsor(issueSp.asset, Some(1), master)
    val burnSp        = burn(issueSp.asset, sender = master, version = TxVersion.V2)
    val reissueSp     = reissue(issueSp.asset, master, 1)
    val issueScr      = issue(master, issueAmount + additionalAmount, script = Some(script))
    val burnScr       = burn(issueScr.asset, sender = master, version = TxVersion.V2)
    val reissueScr    = reissue(issueScr.asset, master, 1)
    val assetScript   = setAssetScript(master, issueScr.asset, script)
    val dataTx        = data(master, Seq(BooleanDataEntry("q", true)))
    val tr1           = transfer(master, acc.toAddress, 10000000000L)
    val tr2           = transfer(master, acc.toAddress, additionalAmount, issueScr.asset)
    val massTransfers = Seq(ParsedTransfer(acc.toAddress, TxNonNegativeAmount.unsafeFrom(1)))
    val mt1           = massTransfer(master, massTransfers, version = TxVersion.V1, fee = 1.waves)
    val mt2           = massTransfer(master, massTransfers, issueScr.asset, fee = 1.waves)
    val l             = lease(master, acc.toAddress, 1)
    val lc            = leaseCancel(l.id(), master)
    val o1 = order(
      OrderType.BUY,
      issueScr.asset,
      issueSp.asset,
      amount = 100000000L,
      price = 100000000L,
      version = Order.V2,
      sender = master,
      matcher = master
    )
    val o2 = order(
      OrderType.SELL,
      issueScr.asset,
      issueSp.asset,
      amount = 100000000L,
      price = 100000000L,
      version = Order.V2,
      sender = acc,
      matcher = master
    )
    val exchange = exchangeFromOrders(o1, o2, master, version = TxVersion.V2, fee = 1.waves)
    val o1a = order(
      OrderType.BUY,
      issueScr.asset,
      issueSp.asset,
      amount = 100000000L,
      price = 100000000L,
      version = Order.V2,
      sender = master,
      matcher = acc
    )
    val o2a = order(
      OrderType.SELL,
      issueScr.asset,
      issueSp.asset,
      amount = 100000000L,
      price = 100000000L,
      version = Order.V2,
      sender = acc,
      matcher = acc
    )
    val exchangea    = exchangeFromOrders(o1a, o2a, acc, version = TxVersion.V2, fee = 1.waves)
    val setContractB = setScript(acc, script)
    val issueScrB    = issue(acc, issueAmount + additionalAmount, script = Some(script))
    val o1b = order(
      OrderType.BUY,
      issueScrB.asset,
      issueScr.asset,
      amount = 100000001L,
      price = 100000001L,
      version = Order.V2,
      sender = master,
      matcher = master
    )
    val o2b = order(
      OrderType.SELL,
      issueScrB.asset,
      issueScr.asset,
      amount = 100000001L,
      price = 100000001L,
      version = Order.V2,
      sender = acc,
      matcher = master
    )
    val exchangeB = exchangeFromOrders(o1b, o2b, master, version = TxVersion.V2, fee = 1.waves)

    val txs = Seq[Transaction](
      setContract,
      issueSp,
      sponsorTx,
      issueScr,
      burnSp,
      burnScr,
      reissueSp,
      reissueScr,
      resetContract,
      assetScript,
      dataTx,
      tr1,
      tr2,
      mt1,
      mt2,
      l,
      lc,
      exchange,
      exchangea,
      issueScrB,
      setContractB,
      exchangeB
    )

    (genesisTx, txs)
  }
}
