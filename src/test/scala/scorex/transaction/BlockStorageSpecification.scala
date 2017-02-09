package scorex.transaction

import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.Account
import scorex.block.Block

import scala.util.{Failure, Success, Try}

class BlockStorageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with PathMockFactory {
  private def mockHistory: History = {
    val history = mock[History]
    (history.appendBlock(_: Block)) expects * onCall { b: Block => Success(Seq(b)) } anyNumberOfTimes()
    history
  }

  private class MockLagonakiState(v: Try[State]) extends LagonakiState {
    override private[transaction] def applyBlock(block: Block): Try[State] = v
    override def included(signature: Array[Byte]): Option[Int] = ???
    override private[transaction] def rollbackTo(height: Int): State = ???
    override def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int]): Long = ???
    override def accountTransactions(account: Account, limit: Int): Seq[_ <: Transaction] = ???
    override def validate(txs: Seq[Transaction], blockTime: Long): Seq[Transaction] = ???
    override def balance(account: Account, height: Int): Long = ???
  }

  property("BlockStorage appendBlock should returns failed try when state.processBlock fails") {
    val f = Failure(new IllegalStateException)

    new BlockStorage {
      override val history: History = mockHistory
      override protected[this] val db: MVStore = new MVStore.Builder().open()
      override val state: LagonakiState = new MockLagonakiState(f)
    }.appendBlock(/*i'm a block*/null) shouldBe f
  }
}
