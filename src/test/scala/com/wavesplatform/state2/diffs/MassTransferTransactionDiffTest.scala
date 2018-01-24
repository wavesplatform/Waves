package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{LeaseInfo, Portfolio}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.Address
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{IssueTransaction, MassTransferTransaction}

class MassTransferTransactionDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def preconditionsAndTransfer(transferCount: Int): Gen[(GenesisTransaction, IssueTransaction, MassTransferTransaction)] = for {
    master <- accountGen
    transferGen = for {
      recipient <- accountGen.map(_.toAddress)
      amount <- Gen.choose(100000L, 1000000000L)
    } yield (recipient, amount)
    transfers <- Gen.listOfN(transferCount, transferGen)
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    (assetIssue: IssueTransaction, _, _) <- issueReissueBurnGeneratorP(ENOUGH_AMT, master)
    maybeAsset <- Gen.option(assetIssue)
    transfer <- massTransferGeneratorP(master, transfers, maybeAsset.map(_.id()))
  } yield (genesis, assetIssue, transfer)

  def testDiff(transferCount: Int) =
    forAll(preconditionsAndTransfer(transferCount)) { case (genesis, issue, transfer) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue))), TestBlock.create(Seq(transfer))) { case (totalDiff, newState) =>
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

  property("MassTransfer preserves balance invariant") {
    import MassTransferTransaction.{maxRecipientCount => Max}
    Seq(0, 1, Max) foreach testDiff // test edge cases
    Gen.choose(2, Max - 1) map testDiff
  }
}
