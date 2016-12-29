package scorex.consensus

import scorex.crypto.encode.Base58
import scorex.transaction.Transaction

object TransactionsOrdering extends Ordering[Transaction] {
  private def orderBy(t: Transaction): (Int, Long, Long, String) = {
    //TODO sort by real value of fee?
    val byFee = t.assetFee._1.foldLeft(-t.assetFee._2)((_, asset) => 0L)
    val byTimestamp = -t.timestamp
    val byAddress = Base58.encode(t.id)
    (t.typeOrdering, byFee, byTimestamp, byAddress)
  }
  override def compare(first: Transaction, second: Transaction): Int = {
    implicitly[Ordering[(Int, Long, Long, String)]].compare(orderBy(first), orderBy(second))
  }
}
