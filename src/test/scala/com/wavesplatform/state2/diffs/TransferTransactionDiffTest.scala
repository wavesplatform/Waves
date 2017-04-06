package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction, Transaction, TransactionGen}

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndTransfer: Gen[(Seq[Transaction], TransferTransaction)] = for {
    master <- accountGen
    recepient <- recipientGen(master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    issue1: IssueTransaction <- issueReissueGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueGeneratorP(ENOUGH_AMT, master).map(_._1)
    preconditions = Seq(genesis, issue1, issue2)
    maybeAsset <- Gen.option(issue1)
    maybeAsset2 <- Gen.option(issue2)
    maybeFeeAsset <- Gen.oneOf(maybeAsset, maybeAsset2)
    transfer <- transferGeneratorP(master, recepient, maybeAsset.map(_.id), maybeFeeAsset.map(_.id)) suchThat (_.recipient.isInstanceOf[Account])
  } yield (preconditions, transfer)

  property("Diff doesn't break invariant") {
    forAll(preconditionsAndTransfer, accountGen) { case ((preconditions: Seq[Transaction], transfer: TransferTransaction), miner: PrivateKeyAccount) =>
      assertDiffEi(Seq(TestBlock(preconditions)), TestBlock(Seq(transfer), miner)) { (totalDiffEi) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiffEi.explicitGet().txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
      }
    }
  }
}
