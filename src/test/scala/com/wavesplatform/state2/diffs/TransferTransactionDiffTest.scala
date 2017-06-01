package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction}

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndTransfer: Gen[(GenesisTransaction, IssueTransaction, IssueTransaction, TransferTransaction)] = for {
    master <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    issue1: IssueTransaction <- issueReissueBurnMakeAssetNameUniqueGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueBurnMakeAssetNameUniqueGeneratorP(ENOUGH_AMT, master).map(_._1)
    maybeAsset <- Gen.option(issue1)
    maybeAsset2 <- Gen.option(issue2)
    maybeFeeAsset <- Gen.oneOf(maybeAsset, maybeAsset2)
    transfer <- transferGeneratorP(master, recepient, maybeAsset.map(_.id.arr), maybeFeeAsset.map(_.id.arr))
  } yield (genesis, issue1, issue2, transfer)

  property("transfers assets to recipient preserving waves invariant") {
    forAll(preconditionsAndTransfer, accountGen) { case ((genesis, issue1, issue2, transfer), miner: PrivateKeyAccount) =>
      assertDiffAndState(Seq(TestBlock(Seq(genesis, issue1, issue2))), TestBlock(Seq(transfer), miner)) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        val recipient: Account = transfer.recipient.asInstanceOf[Account]
        val recipientPortfolio = newState.accountPortfolio(recipient)
        if (transfer.sender.toAccount != recipient) {
          transfer.assetId match {
            case Some(aid) => recipientPortfolio shouldBe Portfolio(0, LeaseInfo.empty, Map(EqByteArray(aid) -> transfer.amount))
            case None => recipientPortfolio shouldBe Portfolio(transfer.amount, LeaseInfo.empty, Map.empty)
          }
        }
      }
    }
  }
}
