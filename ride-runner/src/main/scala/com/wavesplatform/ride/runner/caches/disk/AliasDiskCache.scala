package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.ride.runner.caches.{CacheKey, RemoteData}
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait AliasDiskCache {
  def getAllKeys(fromHeight: Height)(implicit ctx: ReadOnly): Seq[CacheKey.Alias]
  def getAddress(key: CacheKey.Alias)(implicit ctx: ReadOnly): RemoteData[Address]
  def setAddress(atHeight: Height, key: CacheKey.Alias, address: RemoteData[Address])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Vector[CacheKey.Alias]
}
