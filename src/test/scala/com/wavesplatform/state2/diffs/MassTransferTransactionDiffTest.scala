package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state2.{LeaseInfo, Portfolio}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Address, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock.{create => block}
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{IssueTransaction, MassTransferTransaction}

class MassTransferTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 0))

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
  } yield (genesis, master, ts)

  property("MassTransfer preserves balance invariant") {
    def testDiff(transferCount: Int) = {
      val setup = for {
        (genesis, master, ts) <- baseSetup
        transferGen = for {
          recipient <- accountGen.map(_.toAddress)
          amount <- Gen.choose(100000L, 1000000000L)
        } yield (recipient, amount)
        transfers <- Gen.listOfN(transferCount, transferGen)
        (assetIssue: IssueTransaction, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, master)
        maybeAsset <- Gen.option(assetIssue)
        transfer <- massTransferGeneratorP(master, transfers, maybeAsset.map(_.id()))
      } yield (genesis, assetIssue, transfer)

      forAll(setup) { case (genesis, issue, transfer) =>
        assertDiffAndState(Seq(block(Seq(genesis, issue))), block(Seq(transfer)), fs) { case (totalDiff, newState) =>
          val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
          totalPortfolioDiff.balance shouldBe 0
          totalPortfolioDiff.effectiveBalance shouldBe 0
          totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

          val totalAmount = transfer.transfers.map(_._2).sum
          val fees = issue.fee + transfer.fee
          val senderPortfolio = newState.accountPortfolio(transfer.sender)
          transfer.assetId match {
            case Some(aid) => senderPortfolio shouldBe Portfolio(ENOUGH_AMT - fees, LeaseInfo.empty, Map(aid -> (ENOUGH_AMT - totalAmount)))
            case None => senderPortfolio.balance shouldBe (ENOUGH_AMT - fees - totalAmount)
          }
          for ((recipient, amount) <- transfer.transfers) {
            val recipientPortfolio = newState.accountPortfolio(recipient.asInstanceOf[Address])
            if (transfer.sender.toAddress != recipient) {
              transfer.assetId match {
                case Some(aid) => recipientPortfolio shouldBe Portfolio(0, LeaseInfo.empty, Map(aid -> amount))
                case None => recipientPortfolio shouldBe Portfolio(amount, LeaseInfo.empty, Map.empty)
              }
            }
          }
        }
      }
    }

    import MassTransferTransaction.{MaxTransferCount => Max}
    Seq(0, 1, Max) foreach testDiff // test edge cases
    Gen.choose(2, Max - 1) map testDiff
  }

  property("MassTransfer fails on non-existent alias") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      recipient <- aliasGen
      amount <- Gen.choose(100000L, 1000000000L)
      transfer <- massTransferGeneratorP(master, List((recipient, amount)), None)
    } yield (genesis, transfer)

    forAll(setup) { case (genesis, transfer) =>
      assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
        blockDiffEi should produce("AliasNotExists")
      }
    }
  }

  property("MassTransfer fails on non-issued asset") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      recipient <- accountGen.map(_.toAddress)
      amount <- Gen.choose(100000L, 1000000000L)
      assetId <- assetIdGen.filter(_.isDefined)
      transfer <- massTransferGeneratorP(master, List((recipient, amount)), assetId)
    } yield (genesis, transfer)

    forAll(setup) { case (genesis, transfer) =>
      assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
        blockDiffEi should produce("Attempt to transfer unavailable funds")
      }
    }
  }

  property("MassTransfer cannot overspend funds") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      recipients <- Gen.listOfN(2, accountGen.map(acc => (acc.toAddress, ENOUGH_AMT / 2 + 1)))
      (assetIssue: IssueTransaction, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, master)
      maybeAsset <- Gen.option(assetIssue)
      transfer <- massTransferGeneratorP(master, recipients, maybeAsset.map(_.id()))
    } yield (genesis, transfer)

    forAll(setup) { case (genesis, transfer) =>
      assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
        blockDiffEi should produce("Attempt to transfer unavailable funds")
      }
    }
  }

  property("validation fails prior to feature activation") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      transfer <- massTransferGeneratorP(master, List(), None)
    } yield (genesis, transfer)
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 10))

    forAll(setup) { case (genesis, transfer) =>
      assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), settings) { blockDiffEi =>
        blockDiffEi should produce("MassTransfer transaction has not been activated yet")
      }
    }
  }
}
