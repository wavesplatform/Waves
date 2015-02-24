package database

import scorex.transaction.Transaction


trait UnconfirmedTransactionsDatabase {
  def put(tx : Transaction):Boolean
  def getAll():Seq[Transaction]
  def getBySignature(signature:Array[Byte]):Option[Transaction]
  def remove(tx:Transaction)
}
