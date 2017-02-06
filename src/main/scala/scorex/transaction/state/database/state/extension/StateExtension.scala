package scorex.transaction.state.database.state.extension

import scorex.transaction.Transaction
import scorex.transaction.state.database.blockchain.StoredState

trait StateExtension {
  def isValid(storedState: StoredState, tx: Transaction, height: Int): Boolean

  def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit

}
