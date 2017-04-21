package com.wavesplatform.state2

import java.util

import org.h2.mvstore.{MVMap, MVStore}


trait JavaMapStorage {
  def getHeight: Int

  def setHeight(i: Int): Unit

  val transactions: java.util.Map[Array[Byte], (Int, Array[Byte])]
  val portfolios: java.util.Map[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])]
  val assets: java.util.Map[Array[Byte], (Boolean, Long)]
  val accountTransactionIds: java.util.Map[Array[Byte], List[Array[Byte]]]
  val effectiveBalanceSnapshots: util.Map[(Array[Byte], Int), (Long, Long, Long, Long)]
  val paymentTransactionHashes: util.Map[Array[Byte], Array[Byte]]
  val exchangeTransactionsByOrder: util.Map[Array[Byte], List[Array[Byte]]]
  val aliasToAddress: util.Map[String, Array[Byte]]
  val leaseState: util.Map[Array[Byte], Boolean]

  def commit(): Unit
}

class MVStorePrimitiveImpl(db: MVStore) extends JavaMapStorage {

  private val variables: MVMap[String, Int] = db.openMap("variables")
  private val heightKey = "height"

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)

  val transactions: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")

  val portfolios: MVMap[Array[Byte], (Long, (Long, Long), Map[Array[Byte], Long])] = db.openMap("portfolios")

  val assets: MVMap[Array[Byte], (Boolean, Long)] = db.openMap("assets")

  val accountTransactionIds: MVMap[Array[Byte], List[Array[Byte]]] = db.openMap("accountTransactionIds")

  val effectiveBalanceSnapshots: util.Map[(Array[Byte], Int), (Long, Long, Long, Long)] = db.openMap("effectiveBalanceUpdates")

  val paymentTransactionHashes: MVMap[Array[Byte], Array[Byte]] = db.openMap("paymentTransactionHashes")

  val aliasToAddress: MVMap[String, Array[Byte]] = db.openMap("aliasToAddress")

  val exchangeTransactionsByOrder: MVMap[Array[Byte], List[Array[Byte]]] = db.openMap("exchangeTransactionsByOrder")

  val leaseState: MVMap[Array[Byte], Boolean] = db.openMap("leaseState")

  override def commit(): Unit = db.commit()
}
