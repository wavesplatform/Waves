package scorex.transaction.state.database

import scorex.transaction.LagonakiTransaction


trait UnconfirmedTransactionsDatabase {
  def putIfNew(tx: LagonakiTransaction): Boolean

  def all(): Seq[LagonakiTransaction]

  def getBySignature(signature: Array[Byte]): Option[LagonakiTransaction]

  def remove(tx: LagonakiTransaction)
}
