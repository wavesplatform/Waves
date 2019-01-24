package com.wavesplatform.state.diffs

import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class MassTransferTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 0))

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
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
        (assetIssue: IssueTransactionV1, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, master)
        maybeAsset                             <- Gen.option(assetIssue)
        transfer                               <- massTransferGeneratorP(master, transfers, maybeAsset.map(_.id()))
      } yield (genesis, assetIssue, transfer)

      forAll(setup) {
        case (genesis, issue, transfer) =>
          assertDiffAndState(Seq(block(Seq(genesis, issue))), block(Seq(transfer)), fs) {
            case (totalDiff, newState) =>
              assertBalanceInvariant(totalDiff)

              val totalAmount     = transfer.transfers.map(_.amount).sum
              val fees            = issue.fee + transfer.fee
              val senderPortfolio = newState.portfolio(transfer.sender)
              transfer.assetId match {
                case Some(aid) => senderPortfolio shouldBe Portfolio(ENOUGH_AMT - fees, LeaseBalance.empty, Map(aid -> (ENOUGH_AMT - totalAmount)))
                case None      => senderPortfolio.balance shouldBe (ENOUGH_AMT - fees - totalAmount)
              }
              for (ParsedTransfer(recipient, amount) <- transfer.transfers) {
                val recipientPortfolio = newState.portfolio(recipient.asInstanceOf[Address])
                if (transfer.sender.toAddress != recipient) {
                  transfer.assetId match {
                    case Some(aid) => recipientPortfolio shouldBe Portfolio(0, LeaseBalance.empty, Map(aid -> amount))
                    case None      => recipientPortfolio shouldBe Portfolio(amount, LeaseBalance.empty, Map.empty)
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
      transfer          <- massTransferGeneratorP(master, List(ParsedTransfer(recipient, amount)), None)
    } yield (genesis, transfer)

    forAll(setup) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
          blockDiffEi should produce("AliasDoesNotExist")
        }
    }
  }

  property("MassTransfer fails on non-issued asset") {
    val setup = for {
      (genesis, master) <- baseSetup
      recipient         <- accountGen.map(_.toAddress)
      amount            <- Gen.choose(100000L, 1000000000L)
      assetId           <- assetIdGen.filter(_.isDefined)
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
      (assetIssue: IssueTransactionV1, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, master)
      maybeAsset                             <- Gen.option(assetIssue)
      transfer                               <- massTransferGeneratorP(master, recipients, maybeAsset.map(_.id()))
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
      transfer          <- massTransferGeneratorP(master, List(), None)
    } yield (genesis, transfer)
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 10))

    forAll(setup) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), settings) { blockDiffEi =>
          blockDiffEi should produce("MassTransferTransaction has not been activated yet")
        }
    }
  }
}
