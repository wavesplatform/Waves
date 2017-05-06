package com.wavesplatform.state2

import java.util

import com.google.common.primitives.Ints
import com.wavesplatform.state2.StateStorage.SnapshotKey
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.Account

class StateStorage(db: MVStore) {

  private val variables: MVMap[String, Int] = db.openMap("variables")
  private val heightKey = "height"
  private val isDirtyFlag = "isDirty"

  if (variables.get(isDirtyFlag) == 1)
    throw new IllegalArgumentException(s"Persisted state is corrupt." +
      s" Please remove state.dat and restart the node.")

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)

  def setDirty(isDirty: Boolean): Unit = variables.put(isDirtyFlag, if (isDirty) 1 else 0)

  val transactions: util.Map[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")

  val portfolios: util.Map[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])] = db.openMap("portfolios")

  val assets: util.Map[Array[Byte], (Boolean, Long)] = db.openMap("assets")

  val accountTransactionIds: util.Map[Array[Byte], List[Array[Byte]]] = db.openMap("accountTransactionIds")

  val balanceSnapshots: util.Map[SnapshotKey, (Int, Long, Long)] = db.openMap("balanceSnapshots")

  val paymentTransactionHashes: util.Map[Array[Byte], Array[Byte]] = db.openMap("paymentTransactionHashes")

  val aliasToAddress: util.Map[String, Array[Byte]] = db.openMap("aliasToAddress")

  val orderFills: util.Map[Array[Byte], (Long,Long)] = db.openMap("orderFills")

  val leaseState: util.Map[Array[Byte], Boolean] = db.openMap("leaseState")

  val lastUpdateHeight: MVMap[Array[Byte], Int] = db.openMap("lastUpdateHeight")

  val uniqueAssets: MVMap[Array[Byte], Array[Byte]] = db.openMap("uniqueAssets")

  def commit(): Unit = db.commit()

}

object StateStorage {
  type SnapshotKey = Array[Byte]

  def snapshotKey(acc: Account, height: Int): SnapshotKey = acc.bytes ++ Ints.toByteArray(height)

  def dirty[R](p: StateStorage)(f: => R): R = {
    p.setDirty(true)
    val r = f
    p.setDirty(false)
    r
  }
}
