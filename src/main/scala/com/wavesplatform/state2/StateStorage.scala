package com.wavesplatform.state2

import java.io.File

import com.google.common.primitives.Ints
import com.wavesplatform.utils._
import org.h2.mvstore.`type`.ObjectDataType
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.Address
import scorex.utils.LogMVMapBuilder

import scala.util.Try

class StateStorage private(file: Option[File]) extends AutoCloseable {
  import StateStorage._

  val db: MVStore = createMVStore(file)

  private lazy val variables: MVMap[String, Int] = db.openMap("variables")

  def getHeight: Int = Option(variables.get(heightKey)).getOrElse(0)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)

  val transactions: MVMap[ByteStr, (Int, Array[Byte])] = db.openMap("txs", new LogMVMapBuilder[ByteStr, (Int, Array[Byte])]
    .keyType(DataTypes.byteStr).valueType(DataTypes.transactions))

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

  def commit(): Unit = {
    db.commit()
    db.compact(CompactFillRate, CompactMemorySize)
  }

  override def close(): Unit = db.close()
}

object StateStorage {

  private val versionFieldKey = "stateVersion"

  implicit val historyVersion = new Versioned[StateStorage] {
    override val codeVersion: Int = 2
    override def readVersion(t: StateStorage): Option[Int] = Option(t.variables.get(versionFieldKey))
    override def persistVersion(t: StateStorage, vesrion: Int): Unit = {
      t.variables.put(versionFieldKey, vesrion)
      t.db.commit()
    }
  }

  private val CompactFillRate = 80
  private val CompactMemorySize = 19 * 1024 * 1024

  private val heightKey = "height"

  def apply(file: Option[File], dropExisting: Boolean): Try[StateStorage] =
    createWithStore(file, new StateStorage(file), consistencyCheck = (_ :StateStorage) => true, deleteExisting = dropExisting)

  type AccountIdxKey = Array[Byte]

  def accountIndexKey(acc: Address, index: Int): AccountIdxKey = acc.bytes.arr ++ Ints.toByteArray(index)
}
