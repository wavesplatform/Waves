package com.wavesplatform.state2

import java.util

import org.h2.mvstore.{MVMap, MVStore}


trait StateStorage {
  def getHeight: Int

  def setHeight(i: Int): Unit

  def transactions: java.util.Map[Array[Byte], (Int, Array[Byte])]
  def portfolios: java.util.Map[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])]
  def assets: java.util.Map[Array[Byte], (Boolean, Long)]
  def accountTransactionIds: java.util.Map[Array[Byte], List[Array[Byte]]]
  def effectiveBalanceSnapshots: util.Map[(Array[Byte], Int), (Int, Long, Long)]
  def paymentTransactionHashes: util.Map[Array[Byte], Array[Byte]]
  def exchangeTransactionsByOrder: util.Map[Array[Byte], Set[Array[Byte]]]
  def aliasToAddress: util.Map[String, Array[Byte]]
  def leaseState: util.Map[Array[Byte], Boolean]
  def lastUpdateHeight: util.Map[Array[Byte], Int]

  def commit(): Unit
}

class MVStoreStateStorage(db: MVStore) extends StateStorage {

  private val variables: MVMap[String, Int] = db.openMap("variables")
  private val heightKey = "height"

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)

  val transactions: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")

  val portfolios: MVMap[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])] = db.openMap("portfolios")

  val assets: MVMap[Array[Byte], (Boolean, Long)] = db.openMap("assets")

  val accountTransactionIds: MVMap[Array[Byte], List[Array[Byte]]] = db.openMap("accountTransactionIds")

  val effectiveBalanceSnapshots: util.Map[(Array[Byte], Int), (Int, Long, Long)] = db.openMap("effectiveBalanceUpdates")

  val paymentTransactionHashes: MVMap[Array[Byte], Array[Byte]] = db.openMap("paymentTransactionHashes")

  val aliasToAddress: MVMap[String, Array[Byte]] = db.openMap("aliasToAddress")

  val exchangeTransactionsByOrder: MVMap[Array[Byte], Set[Array[Byte]]] = db.openMap("exchangeTransactionsByOrder")

  val leaseState: MVMap[Array[Byte], Boolean] = db.openMap("leaseState")

  val lastUpdateHeight: MVMap[Array[Byte], Int] = db.openMap("lastUpdateHeight")

  override def commit(): Unit = db.commit()
}
