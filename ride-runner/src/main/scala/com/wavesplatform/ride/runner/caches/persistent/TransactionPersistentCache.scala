package com.wavesplatform.ride.runner.caches.persistent

import com.wavesplatform.ride.runner.caches.{CacheKey, RemoteData}
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait TransactionPersistentCache {
  def getHeight(key: CacheKey.Transaction)(implicit ctx: ReadOnly): RemoteData[Height]
  def setHeight(key: CacheKey.Transaction, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def updateHeightIfExist(key: CacheKey.Transaction, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Seq[CacheKey.Transaction]
}
