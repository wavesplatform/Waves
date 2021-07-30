package com.wavesplatform.state.diffs

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Asset, GenesisTransaction}
import org.scalacheck.Gen

class MassTransferTransactionDiffTest extends PropSpec with WithState {

  val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 0))

  val baseSetup: Gen[(GenesisTransaction, KeyPair)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master)

  property("MassTransfer preserves balance invariant") {
    def testDiff(transferCount: Int): Unit = {
      val setup = for {
        (genesis, master) <- baseSetup
        transferGen = for {
          recipient <- accountGen.map(_.toAddress)
          amount    <- Gen.choose(100000L, 1000000000L)
        } yield ParsedTransfer(recipient, amount)
        transfers                              <- Gen.listOfN(transferCount, transferGen)
        (assetIssue: IssueTransaction, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, master)
        maybeAsset                             <- Gen.option(assetIssue.id()).map(Asset.fromCompatId)
        transfer                               <- massTransferGeneratorP(master, transfers, maybeAsset)
      } yield (genesis, assetIssue, transfer)

      forAll(setup) {
        case (genesis, issue, transfer) =>
          assertDiffAndState(Seq(block(Seq(genesis, issue))), block(Seq(transfer)), fs) {
            case (totalDiff, newState) =>
              assertBalanceInvariant(totalDiff)

              val totalAmount = transfer.transfers.map(_.amount).sum
              val fees        = issue.fee + transfer.fee
              transfer.assetId match {
                case aid @ IssuedAsset(_) =>
                  newState.balance(transfer.sender.toAddress) shouldBe ENOUGH_AMT - fees
                  newState.balance(transfer.sender.toAddress, aid) shouldBe ENOUGH_AMT - totalAmount
                case Waves =>
                  newState.balance(transfer.sender.toAddress) shouldBe ENOUGH_AMT - fees - totalAmount
              }
              for (ParsedTransfer(recipient, amount) <- transfer.transfers) {
                if (transfer.sender.toAddress != recipient) {
                  transfer.assetId match {
                    case aid @ IssuedAsset(_) =>
                      newState.balance(recipient.asInstanceOf[Address], aid) shouldBe amount
                    case Waves =>
                      newState.balance(recipient.asInstanceOf[Address]) shouldBe amount
                  }
                }
              }
          }
      }
    }

    import com.wavesplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount => Max}
    Seq(0, 1, Max) foreach testDiff // test edge cases
    Gen.choose(2, Max - 1) map testDiff
  }

  property("MassTransfer fails on non-existent alias") {
    val setup = for {
      (genesis, master) <- baseSetup
      recipient         <- aliasGen
      amount            <- Gen.choose(100000L, 1000000000L)
      transfer          <- massTransferGeneratorP(master, List(ParsedTransfer(recipient, amount)), Waves)
    } yield (genesis, transfer)

    forAll(setup) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
          blockDiffEi should produce("does not exist")
        }
    }
  }

  property("MassTransfer fails on non-issued asset") {
    val setup = for {
      (genesis, master) <- baseSetup
      recipient         <- accountGen.map(_.toAddress)
      amount            <- Gen.choose(100000L, 1000000000L)
      assetId           <- assetIdGen.filter(_.isDefined).map(Asset.fromCompatId)
      transfer          <- massTransferGeneratorP(master, List(ParsedTransfer(recipient, amount)), assetId)
    } yield (genesis, transfer)

    forAll(setup) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
          blockDiffEi should produce("Attempt to transfer unavailable funds")
        }
    }
  }

  property("MassTransfer cannot overspend funds") {
    val setup = for {
      (genesis, master)                      <- baseSetup
      recipients                             <- Gen.listOfN(2, accountGen.map(acc => ParsedTransfer(acc.toAddress, ENOUGH_AMT / 2 + 1)))
      (assetIssue: IssueTransaction, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, master)
      maybeAsset                             <- Gen.option(assetIssue.id()).map(Asset.fromCompatId)
      transfer                               <- massTransferGeneratorP(master, recipients, maybeAsset)
    } yield (genesis, transfer)

    forAll(setup) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
          blockDiffEi should produce("Attempt to transfer unavailable funds")
        }
    }
  }

  property("validation fails prior to feature activation") {
    val setup = for {
      (genesis, master) <- baseSetup
      transfer          <- massTransferGeneratorP(master, List(), Waves)
    } yield (genesis, transfer)
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 10))

    forAll(setup) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), settings) { blockDiffEi =>
          blockDiffEi should produce("Mass Transfer Transaction feature has not been activated yet")
        }
    }
  }
}
