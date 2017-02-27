package scorex.transaction


trait UnconfirmedTransactionsStorage {
  def putIfNew[T <: Transaction](tx: T, txValidator: T => Either[ValidationError, T]): Either[ValidationError, T]

  def all(): Seq[Transaction]

  def getBySignature(signature: Array[Byte]): Option[Transaction]

  def remove(tx: Transaction)
}
