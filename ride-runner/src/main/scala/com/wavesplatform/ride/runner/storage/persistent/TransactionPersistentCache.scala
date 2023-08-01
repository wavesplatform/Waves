package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.ride.runner.storage.{CacheKey, RemoteData}
import com.wavesplatform.state.Height

trait TransactionPersistentCache {
  def getHeight(key: CacheKey.Transaction)(implicit ctx: ReadOnly): RemoteData[Height]
  def setHeight(key: CacheKey.Transaction, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def updateHeightIfExist(key: CacheKey.Transaction, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Seq[CacheKey.Transaction]
}
