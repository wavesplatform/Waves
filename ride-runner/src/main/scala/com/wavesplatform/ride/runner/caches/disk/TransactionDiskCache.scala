package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.mem.MemCacheKey
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait TransactionDiskCache {
  def getHeight(key: MemCacheKey.Transaction)(implicit ctx: ReadOnly): RemoteData[Height]
  def setHeight(key: MemCacheKey.Transaction, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def updateHeightIfExist(key: MemCacheKey.Transaction, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Seq[MemCacheKey.Transaction]
}
