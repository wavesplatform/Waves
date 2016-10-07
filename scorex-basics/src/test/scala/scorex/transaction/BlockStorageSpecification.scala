package scorex.transaction

import scala.util.{Failure, Success, Try}
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.Account
import scorex.block.Block
import scorex.network.BlockSeq

class BlockStorageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with PathMockFactory {
  private def mockHistory: History = {
    val history = mock[History]
    (history.appendBlock(_: Block)) expects * onCall { b: Block => Success(Seq(b)) } anyNumberOfTimes()
    history
  }

  private class MockLagonakiState(v: Try[State]) extends LagonakiState {
    override private[transaction] def processBlock(block: Block): Try[State] = v
    override def validate(txs: Seq[Transaction], height: Option[Int]): Seq[Transaction] = ???
    override def included(signature: Array[Byte], heightOpt: Option[Int]): Option[Int] = ???
    override private[transaction] def rollbackTo(height: Int): State = ???
    override def balance(account: Account, height: Option[Int]): Long = ???
    override def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int]): Long = ???
    override def accountTransactions(account: Account): Array[_ <: Transaction] = ???
  }

  property("BlockStorage appendBlock should returns failed try when state.processBlock fails") {
    val f = Failure(new IllegalStateException)

    new BlockStorage {
      override val history: History = mockHistory
      override protected[this] val db: MVStore = new MVStore.Builder().open()
      override val state: LagonakiState = new MockLagonakiState(f)
      override val blockSeq: BlockSeq = null
    }.appendBlock(/*i'm a block*/null) shouldBe f
  }
}
