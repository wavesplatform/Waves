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

  override def getLeasedSum(address: Address): Long = {
    Option(leasedSumTable.get(address)).getOrElse(0L)
  }

  override def updateLeasedSum(address: Address, value: Long): Unit = {
    leasedSumTable.put(address, value)
  }
}
