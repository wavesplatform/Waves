package com.wavesplatform.state2

import java.util

import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import org.h2.mvstore.MVStore
import org.scalatest.matchers.{MatchResult, Matcher}
import scorex.account.Account
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.ValidationError

import scala.util.{Left, Right}

package object diffs {
  def ensureSenderHasEnoughBalance(s: StateWriter)(sender: Account, assets: List[ByteArray]): Unit = {
    s.applyBlockDiff(new Diff(Map.empty,
      Map(sender -> Portfolio(Long.MaxValue - 1, LeaseInfo.empty, assets.map(a => a -> (Long.MaxValue - 1)).toMap)),
      assets.map(a => a -> AssetInfo(isReissuable = true, Long.MaxValue - 1)).toMap,
      Map.empty
    ).asBlockDiff)
  }

  implicit class EitherExt[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value) => throw new Exception(value.toString)
      case Right(value) => value
    }
  }

  def newState(): StateWriterImpl = new StateWriterImpl(new MVStorePrimitiveImpl(new MVStore.Builder().open()))

  val differ: (StateReader, Block) => Either[ValidationError, BlockDiff] = BlockDiffer(TestFunctionalitySettings.Enabled)
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block)(assertion: Either[ValidationError, BlockDiff] => Unit): Unit = {
    val state = newState()
    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(TestFunctionalitySettings.Enabled)(newState(), preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, block)
    assertion(totalDiff2)
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block)(assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val state = newState()
    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(TestFunctionalitySettings.Enabled)(newState(), preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, block).explicitGet()
    assertion(totalDiff2, new CompositeStateReader(compositeState, totalDiff2))
  }

  class TestStorage extends JavaMapStorage {
    override val transactions = new util.HashMap[Array[Byte], (Int, Array[Byte])]
    override val portfolios = new util.HashMap[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])]
    override val assets = new util.HashMap[Array[Byte], (Boolean, Long)]
    override val accountTransactionIds = new util.HashMap[Array[Byte], List[Array[Byte]]]
    override val effectiveBalanceSnapshots = new util.HashMap[(Array[Byte], Int), (Long, Long)]
    override val paymentTransactionHashes = new util.HashMap[Array[Byte], Array[Byte]]
    override val maxPaymentTransactionTimestampInPreviousBlocks = new util.HashMap[Array[Byte], Long]
    override val aliasToAddress = new util.HashMap[String, Array[Byte]]
    override val exchangeTransactionsByOrder = new util.HashMap[Array[Byte], List[Array[Byte]]]

    var height: Int = 0

    override def getHeight: Int = height

    override def setHeight(i: Int): Unit = {
      height = i
    }

    override def commit(): Unit = ()
  }

  class ProduceError(errorMessage: String) extends Matcher[Either[_, _]] {
    override def apply(ei: Either[_, _]): MatchResult = {
      ei match {
        case r@Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
        case l@Left(_) => MatchResult(matches = l.toString contains errorMessage,
          "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, l))
      }
    }
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}
