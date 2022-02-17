package com.wavesplatform.state.diffs

import cats.kernel.Monoid
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import org.scalatest.Inside

object ScriptsCountTest {
  def calculateLegacy(blockchain: Blockchain, tx: Transaction): Int = {
    import com.wavesplatform.transaction.Asset.IssuedAsset
    import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
    import com.wavesplatform.transaction.assets.{BurnTransaction, ReissueTransaction, SponsorFeeTransaction}
    import com.wavesplatform.transaction.smart.InvokeScriptTransaction
    import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
    import com.wavesplatform.transaction.{Authorized, Transaction}

    val smartAccountRun = tx match {
      case x: Transaction with Authorized if blockchain.hasAccountScript(x.sender.toAddress) => 1
      case _                                                                                 => 0
    }

    val assetIds = tx match {
      case x: TransferTransaction     => x.assetId.fold[Seq[IssuedAsset]](Nil)(Seq(_))
      case x: MassTransferTransaction => x.assetId.fold[Seq[IssuedAsset]](Nil)(Seq(_))
      case x: BurnTransaction         => Seq(x.asset)
      case x: ReissueTransaction      => Seq(x.asset)
      case x: SponsorFeeTransaction   => Seq(x.asset)
      case x: ExchangeTransaction =>
        Seq(
          x.buyOrder.assetPair.amountAsset.fold[Seq[IssuedAsset]](Nil)(Seq(_)),
          x.buyOrder.assetPair.priceAsset.fold[Seq[IssuedAsset]](Nil)(Seq(_))
        ).flatten
      case _ => Seq.empty
    }
    val smartTokenRuns = assetIds.flatMap(blockchain.assetDescription).count(_.script.isDefined)

    val invokeScriptRun = tx match {
      case _: InvokeScriptTransaction => 1
      case _                           => 0
    }

    smartAccountRun + smartTokenRuns + invokeScriptRun
  }
}

//noinspection NameBooleanParameters
class ScriptsCountTest extends PropSpec with WithState with Inside {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id       -> 0,
      BlockchainFeatures.SmartAssets.id         -> 0,
      BlockchainFeatures.SmartAccountTrading.id -> 0,
      BlockchainFeatures.Ride4DApps.id          -> 0,
      BlockchainFeatures.DataTransaction.id     -> 0,
      BlockchainFeatures.MassTransfer.id        -> 0,
      BlockchainFeatures.FeeSponsorship.id      -> 0,
      BlockchainFeatures.Ride4DApps.id          -> Int.MaxValue
    )
  )

  private val fs1 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id       -> 0,
      BlockchainFeatures.SmartAssets.id         -> 0,
      BlockchainFeatures.SmartAccountTrading.id -> 0,
      BlockchainFeatures.Ride4DApps.id          -> 0,
      BlockchainFeatures.DataTransaction.id     -> 0,
      BlockchainFeatures.MassTransfer.id        -> 0,
      BlockchainFeatures.FeeSponsorship.id      -> 0,
      BlockchainFeatures.Ride4DApps.id          -> 0
    )
  )

  val allAllowed: Script = ExprScript(TRUE).explicitGet()

  property("check pre-Ride4DApps scripts run count") {
    val (genesis, txs) = preconditions

    assertDiffAndState(Nil, TestBlock.create(Seq(genesis)), fs) {
      case (_, state) =>
        txs.foldLeft(Diff.empty) { (diff, tx) =>
          val newState = CompositeBlockchain(state, diff)
          val newDiff  = TransactionDiffer(Some(tx.timestamp), tx.timestamp)(newState, tx).resultE.explicitGet()
          val oldRuns  = ScriptsCountTest.calculateLegacy(newState, tx)
          if (newDiff.scriptsRun != oldRuns) throw new IllegalArgumentException(s"$tx ${newDiff.scriptsRun} != $oldRuns")
          Monoid.combine(diff, newDiff)
        }
    }

    assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(txs), fs) {
      case (blockDiff, _) =>
        blockDiff.scriptsRun shouldBe 26
    }
  }

  property("check scripts run count") {
    val (genesis, txs) = preconditions

    assertDiffAndState(
      Seq(TestBlock.create(Seq(genesis))),
      TestBlock.create(txs),
      fs1
    ) {
      case (blockDiff, _) =>
        blockDiff.scriptsRun shouldBe 31
        blockDiff.scriptsComplexity shouldBe (Script.estimate(allAllowed, ScriptEstimatorV2, useContractVerifierLimit = false).explicitGet() * 31)
    }
  }

  private def preconditions: (GenesisTransaction, Seq[Transaction]) = {
    val master = TxHelpers.signer(1)
    val acc = TxHelpers.signer(2)

    val issueAmount = 1000
    val additionalAmount = 1000000000L

    val genesis = TxHelpers.genesis(master.toAddress)
    val setContract = TxHelpers.setScript(master, allAllowed)
    val resetContract = TxHelpers.setScript(master, allAllowed)
    val issueSp = TxHelpers.issue(master, issueAmount + additionalAmount)
    val sponsorTx = TxHelpers.sponsor(issueSp.asset, Some(1), master)
    val burnSp = TxHelpers.burn(issueSp.asset, sender = master, version = TxVersion.V2)
    val reissueSp = TxHelpers.reissue(issueSp.asset, master, 1)
    val issueScr = TxHelpers.issue(master, issueAmount + additionalAmount, script = Some(allAllowed))
    val burnScr = TxHelpers.burn(issueScr.asset, sender = master, version = TxVersion.V2)
    val reissueScr = TxHelpers.reissue(issueScr.asset, master, 1)
    val assetScript = TxHelpers.setAssetScript(master, issueScr.asset, allAllowed)
    val data = TxHelpers.data(master, Seq(BooleanDataEntry("q", true)))
    val tr1 = TxHelpers.transfer(master, acc.toAddress, 10000000000L)
    val tr2 = TxHelpers.transfer(master, acc.toAddress, additionalAmount, issueScr.asset)
    val mt1 = TxHelpers.massTransfer(master, Seq(ParsedTransfer(acc.toAddress, 1)), version = TxVersion.V1)
    val mt2 = TxHelpers.massTransfer(master, Seq(ParsedTransfer(acc.toAddress, 1)), issueScr.asset, version = TxVersion.V1)
    val l = TxHelpers.lease(master, acc.toAddress, 1)
    val lc = TxHelpers.leaseCancel(l.id(), master)
    val o1 = TxHelpers.order(OrderType.BUY, issueScr.asset, issueSp.asset, amount = 100000000L, price = 100000000L, version = TxVersion.V2, sender = master, matcher = master)
    val o2 = TxHelpers.order(OrderType.SELL, issueScr.asset, issueSp.asset, amount = 100000000L, price = 100000000L, version = TxVersion.V2, sender = acc, matcher = master)
    val exchange = TxHelpers.exchange(o1, o2, master, version = TxVersion.V2)
    val o1a = TxHelpers.order(OrderType.BUY, issueScr.asset, issueSp.asset, amount = 100000000L, price = 100000000L, version = TxVersion.V2, sender = master, matcher = acc)
    val o2a = TxHelpers.order(OrderType.SELL, issueScr.asset, issueSp.asset, amount = 100000000L, price = 100000000L, version = TxVersion.V2, sender = acc, matcher = acc)
    val exchangea = TxHelpers.exchange(o1a, o2a, acc, version = TxVersion.V2)
    val setContractB = TxHelpers.setScript(acc, allAllowed)
    val issueScrB = TxHelpers.issue(acc, issueAmount + additionalAmount, script = Some(allAllowed))
    val o1b = TxHelpers.order(OrderType.BUY, issueScrB.asset, issueScr.asset, amount = 100000001L, price = 100000001L, version = TxVersion.V2, sender = master, matcher = master)
    val o2b = TxHelpers.order(OrderType.SELL, issueScrB.asset, issueScr.asset, amount = 100000001L, price = 100000001L, version = TxVersion.V2, sender = acc, matcher = master)
    val exchangeB = TxHelpers.exchange(o1b, o2b, master, version = TxVersion.V2)

    val txs = Seq[Transaction](
      setContract,
      issueSp,       // 1
      sponsorTx,     // 1
      issueScr,      // 1
      burnSp,        // 1
      burnScr,       // 2
      reissueSp,     // 1
      reissueScr,    // 2
      resetContract, // 1
      assetScript,   // 1
      data,          // 1
      tr1,           // 1
      tr2,           // 2
      mt1,           // 1
      mt2,           // 2
      l,             // 1
      lc,            // 1
      exchange,      // 2
      exchangea,     // 1
      issueScrB,
      setContractB,
      exchangeB // 4
    )

    (genesis, txs)
  }
}
