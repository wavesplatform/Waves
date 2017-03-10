package com.wavesplatform.state2

import org.h2.mvstore.{MVMap, MVStore}


trait JavaMapStorage {
  val txs: java.util.Map[Array[Byte], (Int, Array[Byte])]
  val portfolios: java.util.Map[Array[Byte], (Long, Long, Map[Array[Byte], Long])]
}

class MVStorePrimitiveImpl extends JavaMapStorage {
  val db: MVStore = ???
  val txs: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")
  val portfolios: MVMap[Array[Byte], (Long, Long, Map[Array[Byte], Long])] = db.openMap("portfolios")
}
