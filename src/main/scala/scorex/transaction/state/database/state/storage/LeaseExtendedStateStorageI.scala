package scorex.transaction.state.database.state.storage

import scorex.crypto.encode.Base58
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.state.{Address, Row}

trait LeaseExtendedStateStorageI {
  self: StateStorageI =>

  def getLeaseTx(leaseTxId: Array[Byte]): Option[LeaseTransaction] = self.getTransaction(leaseTxId).flatMap {
    case tx: LeaseTransaction => Some(tx)
    case _ => None
  }

  def getExistedLeaseTx(leaseId: Array[Byte]): LeaseTransaction = {
    getLeaseTx(leaseId)
      .getOrElse(throw new RuntimeException(s"There are no lease tx with id ${Base58.encode(leaseId)}"))
  }

  def getLeasedSum(address: Address): Long

  def updateLeasedSum(address: Address, value: Long): Unit
}
