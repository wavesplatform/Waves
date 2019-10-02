package com.wavesplatform.state.diffs

import cats.kernel.Monoid
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state._
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

object ScriptsCountTest {
  def calculateLegacy(blockchain: Blockchain, tx: Transaction): Int = {
    import com.wavesplatform.transaction.Asset.IssuedAsset
    import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
    import com.wavesplatform.transaction.assets.{BurnTransaction, ReissueTransaction, SponsorFeeTransaction}
    import com.wavesplatform.transaction.smart.InvokeScriptTransaction
    import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
    import com.wavesplatform.transaction.{Authorized, Transaction}

    val smartAccountRun = tx match {
      case x: Transaction with Authorized if blockchain.hasScript(x.sender) => 1
      case _                                                                => 0
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
      case tx: InvokeScriptTransaction => 1
      case _                           => 0
    }

    smartAccountRun + smartTokenRuns + invokeScriptRun
  }
}

//noinspection NameBooleanParameters
class ScriptsCountTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {

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
    ))

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
    ))

  val allAllowed = ExprScript(TRUE).explicitGet()

  property("check pre-Ride4DApps scripts run count") {
    forAll(for {
      master <- accountGen
      acc    <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      fee                         = 1000000000L
      setContract                 = SetScriptTransaction.selfSigned(master, Some(allAllowed), fee, ts).explicitGet()
      resetContract               = SetScriptTransaction.selfSigned(master, Some(allAllowed), fee, ts + 1).explicitGet()
      (_, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      issueSp = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, assetName, description, quantity + 1000000000L, decimals, true, None, iFee, timestamp)
        .explicitGet()
      sponsorTx = SponsorFeeTransaction.selfSigned(master, IssuedAsset(issueSp.id()), Some(1), fee, timestamp).explicitGet()
      burnSp    = BurnTransactionV2.selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueSp.id()), 1, fee, timestamp).explicitGet()
      reissueSp = ReissueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueSp.id()), 1, true, fee, timestamp)
        .explicitGet()
      issueScr = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId,
                    master,
                    assetName,
                    description,
                    quantity + 1000000000L,
                    decimals,
                    true,
                    Some(allAllowed),
                    iFee,
                    timestamp)
        .explicitGet()
      burnScr = BurnTransactionV2.selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueScr.id()), 1, fee, timestamp).explicitGet()
      reissueScr = ReissueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueScr.id()), 1, true, fee, timestamp)
        .explicitGet()
      assetScript = SetAssetScriptTransaction
        .create(AddressScheme.current.chainId, master, IssuedAsset(issueScr.id()), Some(allAllowed), fee, timestamp, Proofs.empty)
        .explicitGet()
      data = DataTransaction.selfSigned(master, List(BooleanDataEntry("q", true)), 15000000, timestamp).explicitGet()
      tr1 = TransferTransactionV2
        .selfSigned(Waves, master, acc, 10000000000L, timestamp, Waves, fee, ByteStr(Array()))
        .explicitGet()
      tr2 = TransferTransactionV2
        .selfSigned(IssuedAsset(issueScr.id()), master, acc, 1000000000L, timestamp, Waves, fee, ByteStr(Array()))
        .explicitGet()
      mt1 = MassTransferTransaction.selfSigned(Waves, master, List(ParsedTransfer(acc, 1)), timestamp, fee, ByteStr(Array())).explicitGet()
      mt2 = MassTransferTransaction
        .selfSigned(IssuedAsset(issueScr.id()), master, List(ParsedTransfer(acc, 1)), timestamp, fee, ByteStr(Array()))
        .explicitGet()
      l  = LeaseTransactionV2.selfSigned(master, 1, fee, timestamp, acc).explicitGet()
      lc = LeaseCancelTransactionV2.selfSigned(AddressScheme.current.chainId, master, l.id(), fee, timestamp + 1).explicitGet()

      assetPair = AssetPair(IssuedAsset(issueScr.id()), IssuedAsset(issueSp.id()))
      o1        = Order.buy(master, master, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      o2        = Order.sell(acc, master, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      exchange = ExchangeTransactionV2
        .create(master, o1, o2, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      o1a = Order.buy(master, acc, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      o2a = Order.sell(acc, acc, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      exchangea = ExchangeTransactionV2
        .create(acc, o1a, o2a, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      setContractB = SetScriptTransaction.selfSigned(acc, Some(allAllowed), fee, ts).explicitGet()
      issueScrB = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId,
                    acc,
                    assetName,
                    description,
                    quantity + 1000000000L,
                    decimals,
                    true,
                    Some(allAllowed),
                    iFee,
                    timestamp)
        .explicitGet()
      assetPairB = AssetPair(IssuedAsset(issueScrB.id()), IssuedAsset(issueScr.id()))
      o1b        = Order.buy(master, master, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1, 2: Byte)
      o2b        = Order.sell(acc, master, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1, 2: Byte)
      exchangeB = ExchangeTransactionV2
        .create(master, o1b, o2b, 100000001L, 100000001L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()
    } yield {
      val txs = Seq[Transaction](
        setContract,
        issueSp, // 1
        sponsorTx, // 1
        issueScr, // 1
        burnSp, // 1
        burnScr, // 2
        reissueSp, // 1
        reissueScr, // 2
        resetContract, // 1
        assetScript, // 1
        data, // 1
        tr1, // 1
        tr2, // 2
        mt1, // 1
        mt2, // 2
        l, // 1
        lc, // 1
        exchange, // 2
        exchangea, // 1
        issueScrB,
        setContractB,
        exchangeB // 4
      )

      assertDiffAndState(Nil, TestBlock.create(Seq(genesis)), fs) {
        case (_, state) =>
          txs.foldLeft(Diff.empty) { (diff, tx) =>
            val newState = CompositeBlockchain(state, Some(diff))
            val newDiff  = TransactionDiffer(Some(tx.timestamp), tx.timestamp, state.height)(newState, tx).resultE.explicitGet()
            val oldRuns  = ScriptsCountTest.calculateLegacy(newState, tx)
            if (newDiff.scriptsRun != oldRuns) throw new IllegalArgumentException(s"$tx ${newDiff.scriptsRun} != $oldRuns")
            Monoid.combine(diff, newDiff)
          }
      }

      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(txs), fs) {
        case (blockDiff, _) =>
          blockDiff.scriptsRun shouldBe 26
      }
    }) { x =>
      x
    }
  }

  property("check scripts run count") {
    forAll(for {
      master <- accountGen
      acc    <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      fee                         = 1000000000L
      setContract                 = SetScriptTransaction.selfSigned(master, Some(allAllowed), fee, ts).explicitGet()
      resetContract               = SetScriptTransaction.selfSigned(master, Some(allAllowed), fee, ts + 1).explicitGet()
      (_, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      issueSp = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, assetName, description, quantity + 1000000000L, decimals, true, None, iFee, timestamp)
        .explicitGet()
      sponsorTx = SponsorFeeTransaction.selfSigned(master, IssuedAsset(issueSp.id()), Some(1), fee, timestamp).explicitGet()
      burnSp    = BurnTransactionV2.selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueSp.id()), 1, fee, timestamp).explicitGet()
      reissueSp = ReissueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueSp.id()), 1, true, fee, timestamp)
        .explicitGet()
      issueScr = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId,
                    master,
                    assetName,
                    description,
                    quantity + 1000000000L,
                    decimals,
                    true,
                    Some(allAllowed),
                    iFee,
                    timestamp)
        .explicitGet()
      burnScr = BurnTransactionV2.selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueScr.id()), 1, fee, timestamp).explicitGet()
      reissueScr = ReissueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, IssuedAsset(issueScr.id()), 1, true, fee, timestamp)
        .explicitGet()
      assetScript = SetAssetScriptTransaction
        .create(AddressScheme.current.chainId, master, IssuedAsset(issueScr.id()), Some(allAllowed), fee, timestamp, Proofs.empty)
        .explicitGet()
      data = DataTransaction.selfSigned(master, List(BooleanDataEntry("q", true)), 15000000, timestamp).explicitGet()
      tr1 = TransferTransactionV2
        .selfSigned(Waves, master, acc, 10000000000L, timestamp, Waves, fee, ByteStr(Array()))
        .explicitGet()
      tr2 = TransferTransactionV2
        .selfSigned(IssuedAsset(issueScr.id()), master, acc, 1000000000L, timestamp, Waves, fee, ByteStr(Array()))
        .explicitGet()
      mt1 = MassTransferTransaction.selfSigned(Waves, master, List(ParsedTransfer(acc, 1)), timestamp, fee, ByteStr(Array())).explicitGet()
      mt2 = MassTransferTransaction
        .selfSigned(IssuedAsset(issueScr.id()), master, List(ParsedTransfer(acc, 1)), timestamp, fee, ByteStr(Array()))
        .explicitGet()
      l  = LeaseTransactionV2.selfSigned(master, 1, fee, timestamp, acc).explicitGet()
      lc = LeaseCancelTransactionV2.selfSigned(AddressScheme.current.chainId, master, l.id(), fee, timestamp + 1).explicitGet()

      assetPair = AssetPair(IssuedAsset(issueScr.id()), IssuedAsset(issueSp.id()))
      o1        = Order.buy(master, master, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      o2        = Order.sell(acc, master, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      exchange = ExchangeTransactionV2
        .create(master, o1, o2, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      o1a = Order.buy(master, acc, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      o2a = Order.sell(acc, acc, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1, 2: Byte)
      exchangea = ExchangeTransactionV2
        .create(acc, o1a, o2a, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      setContractB = SetScriptTransaction.selfSigned(acc, Some(allAllowed), fee, ts).explicitGet()
      issueScrB = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId,
                    acc,
                    assetName,
                    description,
                    quantity + 1000000000L,
                    decimals,
                    true,
                    Some(allAllowed),
                    iFee,
                    timestamp)
        .explicitGet()
      assetPairB = AssetPair(IssuedAsset(issueScrB.id()), IssuedAsset(issueScr.id()))
      o1b        = Order.buy(master, master, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1, 2: Byte)
      o2b        = Order.sell(acc, master, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1, 2: Byte)
      exchangeB = ExchangeTransactionV2
        .create(master, o1b, o2b, 100000001L, 100000001L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()
    } yield {
      assertDiffAndState(
        Seq(TestBlock.create(Seq(genesis))),
        TestBlock.create(Seq(
          setContract,
          issueSp, // 1
          sponsorTx, // 1
          issueScr, // 1
          burnSp, // 1
          burnScr, // 2
          reissueSp, // 1
          reissueScr, // 2
          resetContract, // 1
          assetScript, // 2
          data, // 1
          tr1, // 1
          tr2, // 2
          mt1, // 1
          mt2, // 2
          l, // 1
          lc, // 1
          exchange, // 3
          exchangea, // 2
          issueScrB,
          setContractB,
          exchangeB // 5
        )),
        fs1
      ) {
        case (blockDiff, _) =>
          blockDiff.scriptsRun shouldBe 31
          blockDiff.scriptsComplexity shouldBe (Script.estimate(allAllowed, ScriptEstimatorV2).explicitGet() * 31)
      }
    }) { x =>
      x
    }
  }
}
