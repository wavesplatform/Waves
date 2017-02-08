package scorex.transaction.state.database.state.storage

import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.state.{Address, Row}

trait LeaseExtendedStateStorageI {
  self: StateStorageI =>

  def getLeaseTx(leaseTxId: Array[Byte]): Option[LeaseTransaction] = self.getTransaction(leaseTxId).flatMap {
    case tx: LeaseTransaction => Some(tx)
    case _ => None
  }

  def getLeasedSum(address: Address): Long

  def updateLeasedSum(address: Address, delta: Long): Unit

  def getExpiredLeaseTransactions(height: Long): Set[LeaseTransaction]

  def updateExpiredLeaseTransactions(height: Long, txs: Set[LeaseTransaction]): Unit
}
