package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{LeaseBalance, Portfolio}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.Address
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}

class TransferTransactionDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, IssueTransaction, IssueTransaction, TransferTransaction)] = for {
    master <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    maybeAsset <- Gen.option(issue1)
    maybeAsset2 <- Gen.option(issue2)
    maybeFeeAsset <- Gen.oneOf(maybeAsset, maybeAsset2)
    transfer <- transferGeneratorP(master, recepient, maybeAsset.map(_.id()), maybeFeeAsset.map(_.id()))
  } yield (genesis, issue1, issue2, transfer)

  property("transfers assets to recipient preserving waves invariant") {
    forAll(preconditionsAndTransfer) { case ((genesis, issue1, issue2, transfer)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, issue1, issue2))), TestBlock.create(Seq(transfer))) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        val recipient: Address = transfer.recipient.asInstanceOf[Address]
        val recipientPortfolio = newState.portfolio(recipient)
        if (transfer.sender.toAddress != recipient) {
          transfer.assetId match {
            case Some(aid) => recipientPortfolio shouldBe Portfolio(0, LeaseBalance.empty, Map(aid -> transfer.amount))
            case None => recipientPortfolio shouldBe Portfolio(transfer.amount, LeaseBalance.empty, Map.empty)
          }
        }
      }
    }
  }
}
