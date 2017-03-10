package com.wavesplatform.state2

import java.util

import org.h2.mvstore.{MVMap, MVStore}


trait JavaMapStorage {
  val transactions: java.util.Map[Array[Byte], (Int, Array[Byte])]
  val portfolios: java.util.Map[Array[Byte], (Long, Long, Map[Array[Byte], Long])]
  val assets: java.util.Map[Array[Byte], (Boolean, Long)]
}

class MVStorePrimitiveImpl extends JavaMapStorage {
  val db: MVStore = ???
  val transactions: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")
  val portfolios: MVMap[Array[Byte], (Long, Long, Map[Array[Byte], Long])] = db.openMap("portfolios")
  val assets: MVMap[Array[Byte], (Boolean, Long)] = db.openMap("assets")
}
