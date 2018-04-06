package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import com.wavesplatform.lang.{Parser, TypeChecker}
import fastparse.core.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.AddressScheme
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets._
import scorex.transaction.smart.Script
import com.wavesplatform.utils.dummyTypeCheckerContext
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.smart.smartEnabledFS
import scorex.settings.TestFunctionalitySettings
import cats.implicits._

class AssetTransactionsDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  def issueReissueBurnTxs(isReissuable: Boolean): Gen[((GenesisTransaction, IssueTransaction), (ReissueTransaction, BurnTransaction))] =
    for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      ia                     <- positiveLongGen
      ra                     <- positiveLongGen
      ba                     <- positiveLongGen.suchThat(x => x < ia + ra)
      (issue, reissue, burn) <- issueReissueBurnGeneratorP(ia, ra, ba, master) suchThat (_._1.reissuable == isReissuable)
    } yield ((genesis, issue), (reissue, burn))

  property("Issue+Reissue+Burn do not break waves invariant and updates state") {
    forAll(issueReissueBurnTxs(isReissuable = true)) {
      case (((gen, issue), (reissue, burn))) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue, burn))) {
          case (blockDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)

            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets shouldBe Map(reissue.assetId -> (reissue.quantity - burn.amount))

            val totalAssetVolume = issue.quantity + reissue.quantity - burn.amount
            newState.portfolio(issue.sender).assets shouldBe Map(reissue.assetId -> totalAssetVolume)
        }
    }
  }

  property("Cannot reissue/burn non-existing alias") {
    val setup: Gen[(GenesisTransaction, ReissueTransaction, BurnTransaction)] = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      reissue <- reissueGen
      burn    <- burnGen
    } yield (genesis, reissue, burn)

    forAll(setup) {
      case ((gen, reissue, burn)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
        assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(burn))) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
    }
  }

  property("Cannot reissue/burn non-owned alias") {
    val setup = for {
      ((gen, issue), (_, _)) <- issueReissueBurnTxs(isReissuable = true)
      other                  <- accountGen.suchThat(_ != issue.sender.toAddress)
      quantity               <- positiveLongGen
      reissuable2            <- Arbitrary.arbitrary[Boolean]
      fee                    <- Gen.choose(1L, 2000000L)
      timestamp              <- timestampGen
      reissue = ReissueTransaction.create(other, issue.assetId(), quantity, reissuable2, fee, timestamp).right.get
      burn    = BurnTransaction.create(other, issue.assetId(), quantity, fee, timestamp).right.get
    } yield ((gen, issue), reissue, burn)

    forAll(setup) {
      case ((gen, issue), reissue, burn) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(burn))) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
    }
  }

  property("Can burn non-owned alias if feature 'BurnAnyTokens' activated") {
    val setup = for {
      issuer    <- accountGen
      burner    <- accountGen.suchThat(_ != issuer)
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer, ENOUGH_AMT, timestamp).right.get
      (issue, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, issuer)
      assetTransfer <- transferGeneratorP(issuer, burner, issue.assetId().some, None)
      wavesTransfer <- wavesTransferGeneratorP(issuer, burner)
      burn = BurnTransaction.create(burner, issue.assetId(), assetTransfer.amount, wavesTransfer.amount, timestamp).right.get
    } yield (genesis, issue, assetTransfer, wavesTransfer, burn)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.BurnAnyTokens.id -> 0)
        )

    forAll(setup) {
      case (genesis, issue, assetTransfer, wavesTransfer, burn) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue, assetTransfer, wavesTransfer))), TestBlock.create(Seq(burn)), fs) {
          case (_, newState) =>
            newState.portfolio(burn.sender).assets shouldBe Map(burn.assetId -> 0)
        }
    }
  }

  property("Can not reissue > long.max") {
    val setup = for {
      issuer    <- accountGen
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer, ENOUGH_AMT, timestamp).right.get
      assetName   <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      decimals    <- Gen.choose(1: Byte, 8: Byte)
      issue   = IssueTransaction.create(issuer, assetName, description, quantity, decimals, true, fee, timestamp).right.get
      assetId = issue.assetId()
      reissue = ReissueTransaction.create(issuer, assetId, Long.MaxValue, true, 1, timestamp).right.get
    } yield (issuer, assetId, genesis, issue, reissue)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.BurnAnyTokens.id -> 0)
        )

    forAll(setup) {
      case (issuer, assetId, genesis, issue, reissue) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(reissue)), fs) { ei =>
          ei should produce("Asset total value overflow")
        }
    }
  }

  property("Can not total issue > long.max") {
    val setup = for {
      issuer    <- accountGen
      holder    <- accountGen.suchThat(_ != issuer)
      timestamp <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(issuer, ENOUGH_AMT, timestamp).right.get
      assetName   <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      decimals    <- Gen.choose(1: Byte, 8: Byte)
      issue   = IssueTransaction.create(issuer, assetName, description, quantity, decimals, true, fee, timestamp).right.get
      assetId = issue.assetId()
      attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
      transfer = TransferTransaction.create(Some(assetId), issuer, holder, quantity - 1, timestamp, None, fee, attachment).right.get
      reissue  = ReissueTransaction.create(issuer, assetId, (Long.MaxValue - quantity) + 1, true, 1, timestamp).right.get
    } yield (issuer, assetId, genesis, issue, reissue, transfer)

    val fs =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = Map(BlockchainFeatures.BurnAnyTokens.id -> 0)
        )

    forAll(setup) {
      case (issuer, assetId, genesis, issue, reissue, transfer) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue, transfer))), TestBlock.create(Seq(reissue)), fs) { ei =>
          ei should produce("Asset total value overflow")
        }
    }
  }

  property("Cannot reissue non-reissuable alias") {
    forAll(issueReissueBurnTxs(isReissuable = false)) {
      case ((gen, issue), (reissue, _)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
          blockDiffEi should produce("Asset is not reissuable")
        }
    }
  }

  private def createScript(code: String) = {
    val Parsed.Success(expr, _) = Parser(code).get
    Script(TypeChecker(dummyTypeCheckerContext, expr).explicitGet())
  }

  def genesisIssueTransferReissue(code: String): Gen[(Seq[GenesisTransaction], SmartIssueTransaction, TransferTransaction, ReissueTransaction)] =
    for {
      version            <- Gen.oneOf(SmartIssueTransaction.supportedVersions.toSeq)
      timestamp          <- timestampGen
      initialWavesAmount <- Gen.choose(Long.MaxValue / 1000, Long.MaxValue / 100)
      accountA           <- accountGen
      accountB           <- accountGen
      smallFee           <- Gen.choose(1l, 10l)
      genesisTx1 = GenesisTransaction.create(accountA, initialWavesAmount, timestamp).explicitGet()
      genesisTx2 = GenesisTransaction.create(accountB, initialWavesAmount, timestamp).explicitGet()
      reissuable = true
      (_, assetName, description, quantity, decimals, _, _, _) <- issueParamGen
      issue = SmartIssueTransaction
        .selfSigned(version,
                    AddressScheme.current.chainId,
                    accountA,
                    assetName,
                    description,
                    quantity,
                    decimals,
                    reissuable,
                    Some(createScript(code)),
                    smallFee,
                    timestamp + 1)
        .explicitGet()
      assetId = issue.id()
      transfer = TransferTransaction
        .create(Some(assetId), accountA, accountB, issue.quantity, timestamp + 2, None, smallFee, Array.empty)
        .explicitGet()
      reissue = ReissueTransaction.create(accountB, assetId, quantity, reissuable, smallFee, timestamp + 3).explicitGet()
    } yield (Seq(genesisTx1, genesisTx2), issue, transfer, reissue)

  property("Can issue smart asset with script") {
    forAll(for {
      acc        <- accountGen
      genesisGen <- genesisGeneratorP(acc)
      issueGen   <- smartIssueTransactionGen(acc)
    } yield (genesisGen, issueGen)) {
      case (gen, issue) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(issue)), smartEnabledFS) {
          case (blockDiff, newState) =>
            newState.assetDescription(issue.id()) shouldBe Some(
              AssetDescription(issue.sender, issue.name, issue.description, issue.decimals, issue.reissuable, BigInt(issue.quantity), issue.script))
            blockDiff.transactions.get(issue.id()).isDefined shouldBe true
            newState.transactionInfo(issue.id()).isDefined shouldBe true
            newState.transactionInfo(issue.id()).isDefined shouldEqual true
        }
    }
  }

  property("Can transfer when script evaluates to TRUE") {
    forAll(genesisIssueTransferReissue("true")) {
      case (gen, issue, transfer, _) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, transfer)), smartEnabledFS) {
          case (blockDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.assets(issue.id()) shouldEqual issue.quantity
            newState.portfolio(newState.resolveAliasEi(transfer.recipient).right.get).assets(issue.id()) shouldEqual transfer.amount
        }
    }
  }

  property("Cannot transfer when script evaluates to FALSE") {
    forAll(genesisIssueTransferReissue("false")) {
      case (gen, issue, transfer, _) =>
        assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, transfer)), smartEnabledFS)(ei =>
          ei should produce("TransactionNotAllowedByScript"))
    }
  }

  property("Cannot reissue when script evaluates to FALSE") {
    forAll(genesisIssueTransferReissue("false")) {
      case (gen, issue, _, reissue) =>
        assertDiffEi(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, reissue)), smartEnabledFS)(ei =>
          ei should produce("TransactionNotAllowedByScript"))
    }
  }

  property("Can reissue when script evaluates to TRUE even if sender is not original issuer") {
    forAll(genesisIssueTransferReissue("true")) {
      case (gen, issue, _, reissue) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(issue, reissue)), smartEnabledFS) {
          case (blockDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.assets(issue.id()) shouldEqual (issue.quantity + reissue.quantity)
        }
    }
  }

}
