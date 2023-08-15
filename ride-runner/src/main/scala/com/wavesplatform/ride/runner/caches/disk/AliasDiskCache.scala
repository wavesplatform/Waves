package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait AliasDiskCache {
  def getAllKeys(fromHeight: Height)(implicit ctx: ReadOnly): Seq[Alias]
  def getAddress(key: Alias)(implicit ctx: ReadOnly): RemoteData[Address]
  def setAddress(atHeight: Height, key: Alias, address: RemoteData[Address])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Vector[Alias]
}
