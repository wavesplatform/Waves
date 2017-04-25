package com.wavesplatform.state2

import java.util

import com.google.common.primitives.Ints
import com.wavesplatform.state2.StateStorage.SnapshotKey
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.Account

class StateStorage(db: MVStore) {

  private val variables: MVMap[String, Int] = db.openMap("variables")
  private val heightKey = "height"

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)

  val transactions: util.Map[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")

  val portfolios: util.Map[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])] = db.openMap("portfolios")

  val assets: util.Map[Array[Byte], (Boolean, Long)] = db.openMap("assets")

  val accountTransactionIds: util.Map[Array[Byte], List[Array[Byte]]] = db.openMap("accountTransactionIds")

  val balanceSnapshots: util.Map[SnapshotKey, (Int, Long, Long)] = db.openMap("balanceSnapshots")

  val paymentTransactionHashes: util.Map[Array[Byte], Array[Byte]] = db.openMap("paymentTransactionHashes")

  val aliasToAddress: util.Map[String, Array[Byte]] = db.openMap("aliasToAddress")

  val exchangeTransactionsByOrder: util.Map[Array[Byte], Set[Array[Byte]]] = db.openMap("exchangeTransactionsByOrder")

  val leaseState: util.Map[Array[Byte], Boolean] = db.openMap("leaseState")

  val lastUpdateHeight: MVMap[Array[Byte], Int] = db.openMap("lastUpdateHeight")

  def commit(): Unit = db.commit()

}

object StateStorage {
  type SnapshotKey = Array[Byte]

  def snapshotKey(acc: Account, height: Int): SnapshotKey = acc.bytes ++ Ints.toByteArray(height)
}
