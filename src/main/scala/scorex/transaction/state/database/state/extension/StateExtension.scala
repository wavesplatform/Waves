package scorex.transaction.state.database.state.extension

import scorex.transaction.Transaction

trait StateExtension {
  def isValid(tx: Transaction): Boolean

  def process(tx: Transaction, blockTs: Long, height: Int): Unit

}
