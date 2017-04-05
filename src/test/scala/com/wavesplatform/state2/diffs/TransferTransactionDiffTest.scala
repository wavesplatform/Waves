package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader, StateReaderImpl}
import org.h2.mvstore.MVStore
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction, Transaction, TransactionGen, ValidationError}

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val differ: (StateReader, Block) => Either[ValidationError, BlockDiff] = BlockDiffer(TestFunctionalitySettings.Enabled)

  val enoughAmt: Long = Long.MaxValue / 2
  val enoughFee: Long = 100000000L

  def assertDiff(precondition: Block, block: Block)(assertion: Either[ValidationError,BlockDiff] => Unit): Unit = {

    val state = newState()
    val preconditionDiff = differ(state, precondition).explicitGet()
    state.applyBlockDiff(preconditionDiff)

    val totalDiff1 = differ(state, block)
    val totalDiff2 = differ(new CompositeStateReader(newState(), preconditionDiff), block)

    assertion(totalDiff1)
    assertion(totalDiff2)
  }

  val preconditionsAndTransfer: Gen[(Seq[Transaction], TransferTransaction)] = for {
    master <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, enoughAmt, ts).right.get
    issue1: IssueTransaction <- issueReissueGeneratorP(enoughAmt, master).map(_._1)
    issue2: IssueTransaction <- issueReissueGeneratorP(enoughAmt, master).map(_._1)
    preconditions: Seq[Transaction] = Seq(genesis, issue1, issue2)
    maybeAsset: Option[IssueTransaction] <- Gen.option(issue1)
    maybeAsset2: Option[IssueTransaction] <- Gen.option(issue2)
    maybeFeeAsset: Option[IssueTransaction] <- Gen.oneOf(maybeAsset, maybeAsset2)
    transfer: TransferTransaction <- transferGeneratorP(master, maybeAsset.map(_.id), maybeFeeAsset.map(_.id)) suchThat (_.recipient.isInstanceOf[Account])
  } yield (preconditions, transfer)

  property("Block diff for transfer is zero") {
    forAll(preconditionsAndTransfer, accountGen) { case ((preconditions: Seq[Transaction], transfer: TransferTransaction), miner: PrivateKeyAccount) =>

      assertDiff(TestBlock(preconditions), TestBlock(Seq(transfer), miner)) { totalDiff =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.explicitGet().txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
      }
    }
  }
}
