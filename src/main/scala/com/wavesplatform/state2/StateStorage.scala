package com.wavesplatform.state2

import com.google.common.primitives.Ints
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.Account
import scorex.utils.LogMVMapBuilder

class StateStorage private(db: MVStore) {

  import StateStorage._

  private val variables: MVMap[String, Int] = db.openMap("variables")

  private def setPersistedVersion(version: Int) = variables.put(stateVersion, version)

  private def persistedVersion: Option[Int] = Option(variables.get(stateVersion))

  private def setDirty(isDirty: Boolean): Unit = variables.put(isDirtyFlag, if (isDirty) 1 else 0)

  private def dirty(): Boolean = variables.get(isDirtyFlag) == 1

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)

  val transactions: MVMap[ByteArray, (Int, Array[Byte])] = db.openMap("txs", new LogMVMapBuilder[ByteArray, (Int, Array[Byte])]
    .keyType(new EqByteArrayMVStoreDataType))

  val portfolios: MVMap[ByteArray, (Long, (Long, Long), Map[Array[Byte], Long])] = db.openMap("portfolios",
    new LogMVMapBuilder[ByteArray, (Long, (Long, Long), Map[Array[Byte], Long])]
      .keyType(new EqByteArrayMVStoreDataType))


  val assets: MVMap[ByteArray, (Boolean, Long)] = db.openMap("assets", new LogMVMapBuilder[ByteArray, (Boolean, Long)]
    .keyType(new EqByteArrayMVStoreDataType))

  val accountTransactionIds: MVMap[ByteArray, List[Array[Byte]]] = db.openMap("accountTransactionIds",
    new LogMVMapBuilder[ByteArray, List[Array[Byte]]]
      .keyType(new EqByteArrayMVStoreDataType))

  val balanceSnapshots: MVMap[SnapshotKey, (Int, Long, Long)] = db.openMap("balanceSnapshots")

  val paymentTransactionHashes: MVMap[ByteArray, ByteArray] = db.openMap("paymentTransactionHashes",
    new LogMVMapBuilder[ByteArray, ByteArray]
      .keyType(new EqByteArrayMVStoreDataType)
      .valueType(new EqByteArrayMVStoreDataType))

  val aliasToAddress: MVMap[String, ByteArray] = db.openMap("aliasToAddress", new LogMVMapBuilder[String, ByteArray]
    .valueType(new EqByteArrayMVStoreDataType))

  val orderFills: MVMap[ByteArray, (Long, Long)] = db.openMap("orderFills", new LogMVMapBuilder[ByteArray, (Long, Long)]
    .keyType(new EqByteArrayMVStoreDataType))

  val leaseState: MVMap[ByteArray, Boolean] = db.openMap("leaseState", new LogMVMapBuilder[ByteArray, Boolean]
    .keyType(new EqByteArrayMVStoreDataType))

  val lastUpdateHeight: MVMap[ByteArray, Int] = db.openMap("lastUpdateHeight", new LogMVMapBuilder[ByteArray, Int]
    .keyType(new EqByteArrayMVStoreDataType))

  val uniqueAssets: MVMap[ByteArray, ByteArray] = db.openMap("uniqueAssets", new LogMVMapBuilder[ByteArray, ByteArray]
    .keyType(new EqByteArrayMVStoreDataType)
    .valueType(new EqByteArrayMVStoreDataType))

  def commit(): Unit = db.commit()

}

object StateStorage {

  private val VERSION = 1

  private val heightKey = "height"
  private val isDirtyFlag = "isDirty"
  private val stateVersion = "stateVersion"

  def apply(db: MVStore): Either[String, StateStorage] = {
    val s = new StateStorage(db)

    if (s.dirty())
      Left("Persisted state is corrupt")
    else {
      s.persistedVersion match {
        case None =>
          s.setPersistedVersion(VERSION)
          s.commit()
          Right(s)
        case Some(`VERSION`) => Right(s)
        case Some(pv) => Left(s"Persisted state has version $pv, current scheme version is $VERSION")

      }
    }
  }

  type SnapshotKey = Array[Byte]

  def snapshotKey(acc: Account, height: Int): SnapshotKey = acc.bytes.arr ++ Ints.toByteArray(height)

  def dirty[R](p: StateStorage)(f: => R): R = {
    p.setDirty(true)
    val r = f
    p.setDirty(false)
    r
  }
}
