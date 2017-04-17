package scorex.transaction.state.database.state.storage

import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.state.{Address, Row}
import scorex.utils.LogMVMapBuilder

object MVStoreLeaseExtendedStateStorage {
  private val LeasedSum = "LeasedSum"
  private val CanceledLease = "CanceledLease"
}

trait MVStoreLeaseExtendedStateStorage extends LeaseExtendedStateStorageI {
  self: StateStorageI =>

  import MVStoreLeaseExtendedStateStorage._

  def db: MVStore

  private lazy val leasedSumTable: MVMap[String, Long] = db.openMap(LeasedSum, new LogMVMapBuilder[String, Long])
  private lazy val canceledLease: MVMap[String, Boolean] = db.openMap(CanceledLease, new LogMVMapBuilder[String, Boolean])

  override def getLeasedSum(address: Address): Long = {
    Option(leasedSumTable.get(address)).getOrElse(0L)
  }

  override def updateLeasedSum(address: Address, value: Long): Unit = {
    leasedSumTable.put(address, value)
  }

  override def setLeaseTransactionCanceled(leaseTxId: Array[Byte], value: Boolean): Unit = {
    if (value) {
      canceledLease.remove(Base58.encode(leaseTxId))
    } else {
      canceledLease.put(Base58.encode(leaseTxId), false)
    }
  }

  override def isLeaseTransactionCanceled(leaseTxId: Array[Byte]): Boolean = {
    !canceledLease.containsKey(Base58.encode(leaseTxId))
  }

  override def resetLeasesInfo(): Unit = {
    canceledLease.clear()
    leasedSumTable.clear()
  }
}
