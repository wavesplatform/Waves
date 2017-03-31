package com.wavesplatform.state2

import java.util

import org.h2.mvstore.{MVMap, MVStore}


trait JavaMapStorage {
  def getHeight: Int

  def setHeight(i: Int): Unit

  val transactions: java.util.Map[Array[Byte], (Int, Array[Byte])]
  val portfolios: java.util.Map[Array[Byte], (Long, Long, Map[Array[Byte], Long])]
  val assets: java.util.Map[Array[Byte], (Boolean, Long)]
  val accountTransactionIds: java.util.Map[Array[Byte], java.util.List[Array[Byte]]]
  val effectiveBalanceSnapshots: util.Map[(Array[Byte], Int), (Long, Long)]
  val paymentTransactionHashes: util.Map[Array[Byte], Array[Byte]]
  val maxPaymentTransactionTimestampInPreviousBlocks: util.Map[Array[Byte], Long]

  val aliasToAddress: util.Map[String, Array[Byte]]

}

class MVStorePrimitiveImpl(db: MVStore) extends JavaMapStorage {

  private val variables: MVMap[String, Int] = db.openMap("variables")
  private val heightKey = "height"

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)

  val transactions: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")

  val portfolios: MVMap[Array[Byte], (Long, Long, Map[Array[Byte], Long])] = db.openMap("portfolios")

  val assets: MVMap[Array[Byte], (Boolean, Long)] = db.openMap("assets")

  val accountTransactionIds: MVMap[Array[Byte], java.util.List[Array[Byte]]] = db.openMap("accountTransactionIds")

  val effectiveBalanceSnapshots: util.Map[(Array[Byte], Int), (Long, Long)] = db.openMap("effectiveBalanceUpdates")

  val paymentTransactionHashes: MVMap[Array[Byte], Array[Byte]] = db.openMap("paymentTransactionHashes")

  val maxPaymentTransactionTimestampInPreviousBlocks: MVMap[Array[Byte], Long] = db.openMap("maxPaymentTransactionTimestampInPreviousBlocks")

  val aliasToAddress: MVMap[String, Array[Byte]] = db.openMap("aliasToAddress")
}
