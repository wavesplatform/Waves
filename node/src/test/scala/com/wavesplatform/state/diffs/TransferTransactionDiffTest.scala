package com.wavesplatform.state.diffs

import cats.syntax.option._
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxValidationError}
import org.scalacheck.Gen

class TransferTransactionDiffTest extends PropSpec with WithState {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, IssueTransaction, IssueTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    maybeAsset               <- Gen.option(issue1.id()) map Asset.fromCompatId
    maybeAsset2              <- Gen.option(issue2.id()) map Asset.fromCompatId
    maybeFeeAsset            <- Gen.oneOf(maybeAsset, maybeAsset2)
    transferV1               <- transferGeneratorP(master, recepient.toAddress, maybeAsset, maybeFeeAsset)
    transferV2               <- versionedTransferGeneratorP(master, recepient.toAddress, maybeAsset, maybeFeeAsset)
    transfer                 <- Gen.oneOf(transferV1, transferV2)
  } yield (genesis, issue1, issue2, transfer)

  property("transfers assets to recipient preserving waves invariant") {
    forAll(preconditionsAndTransfer) {
      case (genesis, issue1, issue2, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue1, issue2))), TestBlock.create(Seq(transfer))) {
          case (totalDiff, newState) =>
            assertBalanceInvariant(totalDiff)

            val recipient: Address = transfer.recipient.asInstanceOf[Address]
            if (transfer.sender.toAddress != recipient) {
              transfer.assetId match {
                case aid @ IssuedAsset(_) =>
                  newState.balance(recipient) shouldBe 0
                  newState.balance(recipient, aid) shouldBe transfer.amount
                case Waves =>
                  newState.balance(recipient) shouldBe transfer.amount
              }
            }
        }
    }
  }

  val transferWithSmartAssetFee: Gen[(GenesisTransaction, IssueTransaction, IssueTransaction, TransferTransaction)] = {
    for {
      master    <- accountGen
      recepient <- otherAccountGen(master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue: IssueTransaction    <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
      feeIssue: IssueTransaction <- issueV2TransactionGen(master, scriptGen.map(_.some))
      transferV1                 <- transferGeneratorP(master, recepient.toAddress, IssuedAsset(issue.id()), IssuedAsset(feeIssue.id()))
      transferV2                 <- transferGeneratorP(master, recepient.toAddress, IssuedAsset(issue.id()), IssuedAsset(feeIssue.id()))
      transfer                   <- Gen.oneOf(transferV1, transferV2)
    } yield (genesis, issue, feeIssue, transfer)
  }

  property("handle transactions with amount + fee > Long.MaxValue") {
    val precs = for {
      master    <- accountGen
      recepient <- otherAccountGen(candidate = master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue: IssueTransaction <- issueReissueBurnGeneratorP(Long.MaxValue, master).map(_._1)
      asset = IssuedAsset(issue.id())
      transfer = TransferTransaction
        .selfSigned(1.toByte, master, recepient.toAddress, asset, Long.MaxValue, Waves, 100000, ByteStr.empty, ts)
        .explicitGet()
    } yield (genesis, issue, transfer)

    val rdEnabled = TestFunctionalitySettings.Stub

    val rdDisabled = rdEnabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.SmartAccounts.id -> 0,
        BlockchainFeatures.SmartAssets.id   -> 0,
        BlockchainFeatures.FairPoS.id       -> 0
      )
    )

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
          case (_, state) =>
            val diffOrError = TransferTransactionDiff(state, System.currentTimeMillis())(transfer)
            diffOrError shouldBe Left(GenericError("Smart assets can't participate in TransferTransactions as a fee"))
        }
    }
  }
}
