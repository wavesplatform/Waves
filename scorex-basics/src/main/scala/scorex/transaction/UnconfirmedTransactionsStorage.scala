package scorex.transaction


trait UnconfirmedTransactionsStorage {
  val SizeLimit: Int

  def putIfNew(tx: Transaction): Boolean

  def all(): Seq[Transaction]

  def getBySignature(signature: Array[Byte]): Option[Transaction]

  def remove(tx: Transaction)
}
