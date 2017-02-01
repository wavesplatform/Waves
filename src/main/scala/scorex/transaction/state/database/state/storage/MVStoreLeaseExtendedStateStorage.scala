package scorex.transaction.state.database.state.storage

import org.h2.mvstore.MVStore
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.state.{Address, Row}

trait MVStoreLeaseExtendedStateStorage extends LeaseExtendedStateStorageI {
  val db: MVStore

  override def getEffectiveBalanceChanges(key: Address, height: Int): Option[Row] = ???

  override def putEffectiveBalanceChanges(key: Address, height: Int, data: Row): Unit = ???

  override def removeEffectiveBalanceChanges(key: Address, height: Int): Row = ???

  override def getLeaseTx(leaseTxId: Array[Byte]): Option[LeaseTransaction] = ???

  override def getLeasedSum(address: Address): Long = ???

  override def updateLeasedSum(address: Address, delta: Long): Unit = ???

  override def getLastEffectiveBalanceChangeHeight(a: Address): Option[Int] = ???
}
