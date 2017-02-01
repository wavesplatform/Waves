package scorex.transaction.state.database.state.storage

import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.state.{Address, Row}

trait LeaseExtendedStateStorageI {
  def getEffectiveBalanceChanges(key: Address, height: Int): Option[Row]

  def putEffectiveBalanceChanges(key: Address, height: Int, data: Row): Unit

  def removeEffectiveBalanceChanges(key: Address, height: Int): Row

  def getLeaseTx(leaseTxId: Array[Byte]): Option[LeaseTransaction]
}
