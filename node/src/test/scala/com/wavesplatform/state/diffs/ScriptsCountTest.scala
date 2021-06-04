package com.wavesplatform.state.diffs

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
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
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{Inside, PropSpec}
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
      case tx: InvokeScriptTransaction => 1
      case _                           => 0
    }

    smartAccountRun + smartTokenRuns + invokeScriptRun
  }
}

//noinspection NameBooleanParameters
class ScriptsCountTest extends PropSpec with PropertyChecks with WithState with TransactionGen with NoShrink with Inside {

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

  val allAllowed = ExprScript(TRUE).explicitGet()

  property("check pre-Ride4DApps scripts run count") {
    forAll(for {
      master <- accountGen
      acc    <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee                         = 1000000000L
      setContract                 = SetScriptTransaction.selfSigned(1.toByte, master, Some(allAllowed), fee, ts).explicitGet()
      resetContract               = SetScriptTransaction.selfSigned(1.toByte, master, Some(allAllowed), fee, ts + 1).explicitGet()
      (_, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      issueSp = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        assetName,
        description,
        quantity + 1000000000L,
        decimals,
        true,
        None,
        iFee,
        timestamp
      ).signWith(master.privateKey)
      sponsorTx = SponsorFeeTransaction.selfSigned(1.toByte, master, IssuedAsset(issueSp.id()), Some(1), fee, timestamp).explicitGet()
      burnSp    = BurnTransaction.selfSigned(2.toByte, master, IssuedAsset(issueSp.id()), 1, fee, timestamp).explicitGet()
      reissueSp = ReissueTransaction
        .selfSigned(2.toByte, master, IssuedAsset(issueSp.id()), 1, true, fee, timestamp)
        .explicitGet()
      issueScr = IssueTransaction(
        TxVersion.V2,
        master.publicKey,
        assetName,
        description,
        quantity + 1000000000L,
        decimals,
        true,
        Some(allAllowed),
        iFee,
        timestamp
      ).signWith(master.privateKey)
      burnScr = BurnTransaction.selfSigned(2.toByte, master, IssuedAsset(issueScr.id()), 1, fee, timestamp).explicitGet()
      reissueScr = ReissueTransaction
        .selfSigned(2.toByte, master, IssuedAsset(issueScr.id()), 1, true, fee, timestamp)
        .explicitGet()
      assetScript = SetAssetScriptTransaction
        .create(1.toByte, master.publicKey, IssuedAsset(issueScr.id()), Some(allAllowed), fee, timestamp, Proofs.empty)
        .explicitGet()
      data = DataTransaction.selfSigned(1.toByte, master, List(BooleanDataEntry("q", true)), 15000000, timestamp).explicitGet()
      tr1 = TransferTransaction.selfSigned(2.toByte, master, acc.toAddress, Waves, 10000000000L, Waves, fee, ByteStr.empty, timestamp)
        .explicitGet()
      tr2 = TransferTransaction
        .selfSigned(2.toByte, master, acc.toAddress, IssuedAsset(issueScr.id()), 1000000000L, Waves, fee, ByteStr.empty, timestamp)
        .explicitGet()
      mt1 = MassTransferTransaction
        .selfSigned(1.toByte, master, Waves, List(ParsedTransfer(acc.toAddress, 1)), fee, timestamp, ByteStr.empty)
        .explicitGet()
      mt2 = MassTransferTransaction
        .selfSigned(1.toByte, master, IssuedAsset(issueScr.id()), List(ParsedTransfer(acc.toAddress, 1)), fee, timestamp, ByteStr.empty)
        .explicitGet()
      l  = LeaseTransaction.selfSigned(2.toByte, master, acc.toAddress, 1L, fee, timestamp).explicitGet()
      lc = LeaseCancelTransaction.selfSigned(2.toByte, master, l.id(), fee, timestamp + 1).explicitGet()

      assetPair = AssetPair(IssuedAsset(issueScr.id()), IssuedAsset(issueSp.id()))
      o1        = Order.buy(2: Byte, master, master.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      o2        = Order.sell(2: Byte, acc, master.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      exchange = ExchangeTransaction
        .signed(TxVersion.V2, master.privateKey, o1, o2, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      o1a = Order.buy(2: Byte, master, acc.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      o2a = Order.sell(2: Byte, acc, acc.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      exchangea = ExchangeTransaction
        .signed(TxVersion.V2, acc.privateKey, o1a, o2a, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      setContractB = SetScriptTransaction.selfSigned(1.toByte, acc, Some(allAllowed), fee, ts).explicitGet()
      issueScrB = IssueTransaction(
          TxVersion.V2,
          acc.publicKey,
          assetName,
          description,
          quantity + 1000000000L,
          decimals,
          true,
          Some(allAllowed),
          iFee,
          timestamp
        ).signWith(acc.privateKey)
      assetPairB = AssetPair(IssuedAsset(issueScrB.id()), IssuedAsset(issueScr.id()))
      o1b        = Order.buy(2: Byte, master, master.publicKey, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1)
      o2b        = Order.sell(2: Byte, acc, master.publicKey, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1)
      exchangeB = ExchangeTransaction
        .signed(TxVersion.V2, master.privateKey, o1b, o2b, 100000001L, 100000001L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()
    } yield {
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
    }) { x =>
      x
    }
  }

  property("check scripts run count") {
    forAll(for {
      master <- accountGen
      acc    <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee                         = 1000000000L
      setContract                 = SetScriptTransaction.selfSigned(1.toByte, master, Some(allAllowed), fee, ts).explicitGet()
      resetContract               = SetScriptTransaction.selfSigned(1.toByte, master, Some(allAllowed), fee, ts + 1).explicitGet()
      (_, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      issueSp = IssueTransaction(
          TxVersion.V2,
          master.publicKey,
          assetName,
          description,
          quantity + 1000000000L,
          decimals,
          true,
          None,
          iFee,
          timestamp
        )
        .signWith(master.privateKey)
      sponsorTx = SponsorFeeTransaction.selfSigned(1.toByte, master, IssuedAsset(issueSp.id()), Some(1), fee, timestamp).explicitGet()
      burnSp    = BurnTransaction.selfSigned(2.toByte, master, IssuedAsset(issueSp.id()), 1, fee, timestamp).explicitGet()
      reissueSp = ReissueTransaction
        .selfSigned(2.toByte, master, IssuedAsset(issueSp.id()), 1, true, fee, timestamp)
        .explicitGet()
      issueScr = IssueTransaction(
          TxVersion.V2,
          master.publicKey,
          assetName,
          description,
          quantity + 1000000000L,
          decimals,
          true,
          Some(allAllowed),
          iFee,
          timestamp
        ).signWith(master.privateKey)

      burnScr = BurnTransaction.selfSigned(2.toByte, master, IssuedAsset(issueScr.id()), 1, fee, timestamp).explicitGet()
      reissueScr = ReissueTransaction
        .selfSigned(2.toByte, master, IssuedAsset(issueScr.id()), 1, true, fee, timestamp)
        .explicitGet()
      assetScript = SetAssetScriptTransaction
        .create(1.toByte, master.publicKey, IssuedAsset(issueScr.id()), Some(allAllowed), fee, timestamp, Proofs.empty)
        .explicitGet()
      data = DataTransaction.selfSigned(1.toByte, master, List(BooleanDataEntry("q", true)), 15000000, timestamp).explicitGet()
      tr1 = TransferTransaction
        .selfSigned(2.toByte, master, acc.toAddress, Waves, 10000000000L, Waves, fee, ByteStr.empty, timestamp)
        .explicitGet()
      tr2 = TransferTransaction
        .selfSigned(2.toByte, master, acc.toAddress, IssuedAsset(issueScr.id()), 1000000000L, Waves, fee, ByteStr.empty, timestamp)
        .explicitGet()
      mt1 = MassTransferTransaction
        .selfSigned(1.toByte, master, Waves, List(ParsedTransfer(acc.toAddress, 1)), fee, timestamp, ByteStr.empty)
        .explicitGet()
      mt2 = MassTransferTransaction
        .selfSigned(1.toByte, master, IssuedAsset(issueScr.id()), List(ParsedTransfer(acc.toAddress, 1)), fee, timestamp, ByteStr.empty)
        .explicitGet()
      l  = LeaseTransaction.selfSigned(2.toByte, master, acc.toAddress, 1, fee, timestamp).explicitGet()
      lc = LeaseCancelTransaction.selfSigned(2.toByte, master, l.id(), fee, timestamp + 1).explicitGet()

      assetPair = AssetPair(IssuedAsset(issueScr.id()), IssuedAsset(issueSp.id()))
      o1        = Order.buy(2: Byte, master, master.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      o2        = Order.sell(2: Byte, acc, master.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      exchange = ExchangeTransaction
        .signed(TxVersion.V2, master.privateKey, o1, o2, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      o1a = Order.buy(2: Byte, master, acc.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      o2a = Order.sell(2: Byte, acc, acc.publicKey, assetPair, 100000000L, 100000000L, timestamp, 10000L, 1)
      exchangea = ExchangeTransaction
        .signed(TxVersion.V2, acc.privateKey, o1a, o2a, 100000000L, 100000000L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()

      setContractB = SetScriptTransaction.selfSigned(1.toByte, acc, Some(allAllowed), fee, ts).explicitGet()
      issueScrB = IssueTransaction(
        TxVersion.V2,
        acc.publicKey,
        assetName,
        description,
        quantity + 1000000000L,
        decimals,
        true,
        Some(allAllowed),
        iFee,
        timestamp
      ).signWith(acc.privateKey)
      assetPairB = AssetPair(IssuedAsset(issueScrB.id()), IssuedAsset(issueScr.id()))
      o1b        = Order.buy(2: Byte, master, master.publicKey, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1)
      o2b        = Order.sell(2: Byte, acc, master.publicKey, assetPairB, 100000001L, 100000001L, timestamp, 10000L, 1)
      exchangeB = ExchangeTransaction
        .signed(TxVersion.V2, master.privateKey, o1b, o2b, 100000001L, 100000001L, 1, 1, (1 + 1) / 2, 10000L - 100)
        .explicitGet()
    } yield {
      assertDiffAndState(
        Seq(TestBlock.create(Seq(genesis))),
        TestBlock.create(
          Seq(
            setContract,
            issueSp,       // 1
            sponsorTx,     // 1
            issueScr,      // 1
            burnSp,        // 1
            burnScr,       // 2
            reissueSp,     // 1
            reissueScr,    // 2
            resetContract, // 1
            assetScript,   // 2
            data,          // 1
            tr1,           // 1
            tr2,           // 2
            mt1,           // 1
            mt2,           // 2
            l,             // 1
            lc,            // 1
            exchange,      // 3
            exchangea,     // 2
            issueScrB,
            setContractB,
            exchangeB // 5
          )
        ),
        fs1
      ) {
        case (blockDiff, _) =>
          blockDiff.scriptsRun shouldBe 31
          blockDiff.scriptsComplexity shouldBe (Script.estimate(allAllowed, ScriptEstimatorV2, useContractVerifierLimit = false).explicitGet() * 31)
      }
    }) { x =>
      x
    }
  }
}
