package scorex.consensus

import scorex.crypto.encode.Base58
import scorex.transaction.Transaction

object TransactionsOrdering {
  trait WavesOrdering extends Ordering[Transaction] {
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Long, Long, String) = {
      val byFee = t.assetFee._1 match {
        case Some(assetId) => 0
        case None => -t.assetFee._2
      }
      val byTimestamp = txTimestampOrder(t.timestamp)
      val byTxId = t.id.base58

      (byFee, byTimestamp, byTxId)
    }
    override def compare(first: Transaction, second: Transaction): Int = {
      implicitly[Ordering[(Long, Long, String)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends WavesOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long = -ts
  }

  object InUTXPool extends WavesOrdering {
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
