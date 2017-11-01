package com.wavesplatform.state2

import java.io.File

import com.google.common.primitives.Ints
import com.wavesplatform.utils._
import org.h2.mvstore.`type`.ObjectDataType
import org.h2.mvstore.MVMap
import scorex.account.Address
import scorex.utils.LogMVMapBuilder

import scala.util.Try

class StateStorage private(file: Option[File]) extends VariablesStorage(createStore(file)) with VersionableStorage with AutoCloseable {

  import StateStorage._

  override protected val Version = 2

  def getHeight: Int = getInt(heightKey).getOrElse(0)

  def setHeight(i: Int): Unit = putInt(heightKey, i)

  val transactions: MVMap[ByteStr, (Int, Array[Byte])] = db.openMap("txs", new LogMVMapBuilder[ByteStr, (Int, Array[Byte])]
    .keyType(DataTypes.byteStr).valueType(DataTypes.tupleIntByteArray))

  val wavesBalance: MVMap[ByteStr, (Long, Long, Long)] = db.openMap("wavesBalance",
    new LogMVMapBuilder[ByteStr, (Long, Long, Long)]
      .keyType(DataTypes.byteStr).valueType(DataTypes.waves))

  val assetBalance = new MultiKeyMap[Long](db, new ObjectDataType(), "assetBalance")

  val assets: MVMap[ByteStr, (Boolean, Long)] = db.openMap("assets",
    new LogMVMapBuilder[ByteStr, (Boolean, Long)].keyType(DataTypes.byteStr).valueType(DataTypes.assets))

  val accountTransactionIds: MVMap[AccountIdxKey, ByteStr] = db.openMap("accountTransactionIds",
    new LogMVMapBuilder[AccountIdxKey, ByteStr].valueType(DataTypes.byteStr))

  val accountTransactionsLengths: MVMap[ByteStr, Int] = db.openMap("accountTransactionsLengths",
    new LogMVMapBuilder[ByteStr, Int].keyType(DataTypes.byteStr))

  val balanceSnapshots: MVMap[AccountIdxKey, (Int, Long, Long)] = db.openMap("balanceSnapshots",
    new LogMVMapBuilder[AccountIdxKey, (Int, Long, Long)].valueType(DataTypes.balanceSnapshots))

  val paymentTransactionHashes: MVMap[ByteStr, ByteStr] = db.openMap("paymentTransactionHashes",
    new LogMVMapBuilder[ByteStr, ByteStr]
      .keyType(DataTypes.byteStr)
      .valueType(DataTypes.byteStr))

  val aliasToAddress: MVMap[String, ByteStr] = db.openMap("aliasToAddress", new LogMVMapBuilder[String, ByteStr]
    .valueType(DataTypes.byteStr))

  val orderFills: MVMap[ByteStr, (Long, Long)] = db.openMap("orderFills", new LogMVMapBuilder[ByteStr, (Long, Long)]
    .keyType(DataTypes.byteStr).valueType(DataTypes.orderFills))

  val leaseState: MVMap[ByteStr, Boolean] = db.openMap("leaseState", new LogMVMapBuilder[ByteStr, Boolean]
    .keyType(DataTypes.byteStr))

  val lastBalanceSnapshotHeight: MVMap[ByteStr, Int] = db.openMap("lastUpdateHeight", new LogMVMapBuilder[ByteStr, Int]
    .keyType(DataTypes.byteStr))

  def commit(compact: Boolean): Unit = {
    db.commit()
    if (compact)
      db.compact(CompactFillRate, CompactMemorySize)
  }

  override def close(): Unit = db.close()
}

object StateStorage {
  private val CompactFillRate = 80
  private val CompactMemorySize = 19 * 1024 * 1024

  private val heightKey = "height"

  def apply(file: Option[File], dropExisting: Boolean): Try[StateStorage] =
    createWithStore(file, new StateStorage(file), deleteExisting = dropExisting)

  type AccountIdxKey = Array[Byte]

  def accountIndexKey(acc: Address, index: Int): AccountIdxKey = acc.bytes.arr ++ Ints.toByteArray(index)
}
