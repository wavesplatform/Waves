package scorex.state.database

import scorex.transaction.Transaction


trait UnconfirmedTransactionsDatabase {
  def putIfNew(tx: Transaction): Boolean

  def all(): Seq[Transaction]

  def getBySignature(signature: Array[Byte]): Option[Transaction]

  def remove(tx: Transaction)
}
