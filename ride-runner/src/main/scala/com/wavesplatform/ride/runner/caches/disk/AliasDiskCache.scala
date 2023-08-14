package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.caches.mem.MemCacheKey
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait AliasDiskCache {
  def getAllKeys(fromHeight: Height)(implicit ctx: ReadOnly): Seq[MemCacheKey.Alias]
  def getAddress(key: MemCacheKey.Alias)(implicit ctx: ReadOnly): RemoteData[Address]
  def setAddress(atHeight: Height, key: MemCacheKey.Alias, address: RemoteData[Address])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Vector[MemCacheKey.Alias]
}
