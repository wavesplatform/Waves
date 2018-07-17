package scorex.transaction

trait VersionedTransaction {
  def version: Byte
}
