package scorex.transaction


trait UnconfirmedTransactionsStorage {
  def putIfNew(tx: Transaction, txValidator: Transaction => Boolean = _ => true): Boolean

  def all(): Seq[Transaction]

  def getBySignature(signature: Array[Byte]): Option[Transaction]

  def remove(tx: Transaction)
}
