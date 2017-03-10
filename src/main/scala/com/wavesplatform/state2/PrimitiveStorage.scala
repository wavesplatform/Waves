package com.wavesplatform.state2

import org.h2.mvstore.{MVMap, MVStore}


trait PrimitiveStorage {
  val txs: java.util.Map[Array[Byte], (Int, Array[Byte])]
}

class MVStorePrimitiveImpl extends PrimitiveStorage {
  val db: MVStore = ???
  val txs: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")
}
