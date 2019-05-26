package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxValidationError}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, IssueTransaction, IssueTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    maybeAsset               <- Gen.option(issue1.id()) map Asset.fromCompatId
    maybeAsset2              <- Gen.option(issue2.id()) map Asset.fromCompatId
    maybeFeeAsset            <- Gen.oneOf(maybeAsset, maybeAsset2)
    transferV1               <- transferGeneratorP(master, recepient, maybeAsset, maybeFeeAsset)
    transferV2               <- versionedTransferGeneratorP(master, recepient, maybeAsset, maybeFeeAsset)
    transfer                 <- Gen.oneOf(transferV1, transferV2)
  } yield (genesis, issue1, issue2, transfer)

  property("transfers assets to recipient preserving waves invariant") {
    forAll(preconditionsAndTransfer) {
      case ((genesis, issue1, issue2, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue1, issue2))), TestBlock.create(Seq(transfer))) {
          case (totalDiff, newState) =>
            assertBalanceInvariant(totalDiff)

            val recipient: Address = transfer.recipient.asInstanceOf[Address]
            val recipientPortfolio = newState.portfolio(recipient)
            if (transfer.sender.toAddress != recipient) {
              transfer.assetId match {
                case aid @ IssuedAsset(_) => recipientPortfolio shouldBe Portfolio(0, LeaseBalance.empty, Map(aid -> transfer.amount))
                case Waves                => recipientPortfolio shouldBe Portfolio(transfer.amount, LeaseBalance.empty, Map.empty)
              }
            }
        }
    }
  }

  val transferWithSmartAssetFee: Gen[(GenesisTransaction, IssueTransaction, IssueTransactionV2, TransferTransaction)] = {
    for {
      master    <- accountGen
      recepient <- otherAccountGen(master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      issue: IssueTransaction      <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
      feeIssue: IssueTransactionV2 <- smartIssueTransactionGen(master, scriptGen.map(_.some))
      transferV1                   <- transferGeneratorP(master, recepient, IssuedAsset(issue.id()), IssuedAsset(feeIssue.id()))
      transferV2                   <- transferGeneratorP(master, recepient, IssuedAsset(issue.id()), IssuedAsset(feeIssue.id()))
      transfer                     <- Gen.oneOf(transferV1, transferV2)
    } yield (genesis, issue, feeIssue, transfer)
  }

  property("handle transactions with amount + fee > Long.MaxValue") {
    val precs = for {
      master    <- accountGen
      recepient <- otherAccountGen(candidate = master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      issue: IssueTransaction <- issueReissueBurnGeneratorP(Long.MaxValue, master).map(_._1)
      asset    = IssuedAsset(issue.id())
      transfer = TransferTransactionV1.selfSigned(asset, master, recepient, Long.MaxValue, ts, Waves, 100000, Array.emptyByteArray).explicitGet()
    } yield (genesis, issue, transfer)

    val rdEnabled = TestFunctionalitySettings.Stub

    val rdDisabled = rdEnabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.SmartAccounts.id -> 0,
        BlockchainFeatures.SmartAssets.id   -> 0,
        BlockchainFeatures.FairPoS.id       -> 0
      ))

    forAll(precs) {
      case (genesis, issue, transfer) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(transfer)), rdEnabled) { diffEi =>
          diffEi shouldBe an[Right[_, _]]
        }

        assertDiffEi(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(transfer)), rdDisabled) { diffEi =>
          diffEi shouldBe Left(TransactionDiffer.TransactionValidationError(TxValidationError.OverflowError, transfer))
        }
    }
  }

  property("fails, if smart asset used as a fee") {
    import smart._

    forAll(transferWithSmartAssetFee) {
      case (genesis, issue, fee, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(issue, fee)), smartEnabledFS) {
          case (_, state) => {
            val diffOrError = TransferTransactionDiff(state, state.height, System.currentTimeMillis())(transfer)
            diffOrError shouldBe Left(GenericError("Smart assets can't participate in TransferTransactions as a fee"))
          }
        }
    }
  }
}
