package scorex.transaction.state.database.state.storage

import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.state.{Address, Row}
import scorex.utils.LogMVMapBuilder

object MVStoreLeaseExtendedStateStorage {
  private val ExpiredTableName = "ExpiredLease"
  private val LeasedSum = "LeasedSum"
}

trait MVStoreLeaseExtendedStateStorage extends LeaseExtendedStateStorageI {
  self: StateStorageI =>

  import MVStoreLeaseExtendedStateStorage._

  def db: MVStore

  private lazy val leasedSumTable: MVMap[String, Long] = db.openMap(LeasedSum, new LogMVMapBuilder[String, Long])

  private lazy val expiredLeaseTable: MVMap[Long, Set[String]] = db.openMap(ExpiredTableName, new LogMVMapBuilder[Long, Set[String]])

  override def getLeasedSum(address: Address): Long = {
    Option(leasedSumTable.get(address)).getOrElse(0L)
  }

  override def updateLeasedSum(address: Address, value: Long): Unit = {
    leasedSumTable.put(address, value)
  }

  override def getExpiredLeaseTransactions(height: Long): Set[LeaseTransaction] = {
    Option(expiredLeaseTable.get(height))
      .map(_.map(id => getLeaseTx(Base58.decode(id).get).get))
      .getOrElse(Set.empty[LeaseTransaction])
  }

  override def addExpirationForLeaseTransactions(tx: LeaseTransaction): Unit = {
    val oldSet = Option(expiredLeaseTable.get(tx.untilBlock)).getOrElse(Set.empty)
    expiredLeaseTable.put(tx.untilBlock, oldSet + Base58.encode(tx.id))
  }

  override def removeAllLeaseExpirations(tx: LeaseTransaction): Unit = {
    expiredLeaseTable.remove(tx.untilBlock)
  }

  override def removeLeaseTransactionExpiration(tx: LeaseTransaction): Unit = {
    val oldSet = Option(expiredLeaseTable.get(tx.untilBlock)).getOrElse(Set.empty)
    expiredLeaseTable.remove(tx.untilBlock, oldSet - Base58.encode(tx.id))
  }
}
