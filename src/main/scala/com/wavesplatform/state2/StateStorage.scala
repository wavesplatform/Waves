package com.wavesplatform.state2

import com.google.common.primitives.Ints
import com.wavesplatform.state2.StateStorage.SnapshotKey
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.Account

class StateStorage private(db: MVStore) {

  private val variables: MVMap[String, Int] = db.openMap("variables")
  private val heightKey = "height"
  private val isDirtyFlag = "isDirty"

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = {
    val _ = variables.put(heightKey, i)
  }

  def setDirty(isDirty: Boolean): Unit = {
    val _ = variables.put(isDirtyFlag, if (isDirty) 1 else 0)
  }

  val transactions: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")

  val portfolios: MVMap[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])] = db.openMap("portfolios")

  val assets: MVMap[Array[Byte], (Boolean, Long)] = db.openMap("assets")

  val accountTransactionIds: MVMap[Array[Byte], List[Array[Byte]]] = db.openMap("accountTransactionIds")

  val balanceSnapshots: MVMap[SnapshotKey, (Int, Long, Long)] = db.openMap("balanceSnapshots")

  val paymentTransactionHashes: MVMap[Array[Byte], Array[Byte]] = db.openMap("paymentTransactionHashes")

  val aliasToAddress: MVMap[String, Array[Byte]] = db.openMap("aliasToAddress")

  val orderFills: MVMap[Array[Byte], (Long, Long)] = db.openMap("orderFills")

  val leaseState: MVMap[Array[Byte], Boolean] = db.openMap("leaseState")

  val lastUpdateHeight: MVMap[Array[Byte], Int] = db.openMap("lastUpdateHeight")

  val uniqueAssets: MVMap[Array[Byte], Array[Byte]] = db.openMap("uniqueAssets")

  def commit(): Unit = {
    val _ = db.commit()
  }

}

object StateStorage {

  def apply(db: MVStore): Either[String, StateStorage] = {
    val s = new StateStorage(db)

    if (s.variables.get(s.isDirtyFlag) == 1)
      Left(s"Persisted state is corrupt")
    else Right(s)
  }

  type SnapshotKey = Array[Byte]

  def snapshotKey(acc: Account, height: Int): SnapshotKey = acc.bytes ++ Ints.toByteArray(height)

  def dirty[R](p: StateStorage)(f: => R): R = {
    p.setDirty(true)
    val r = f
    p.setDirty(false)
    r
  }
}
