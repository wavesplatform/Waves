package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait MaliciousMinerBanHeightsDiskCache {
  def append(address: Address, height: Height): Seq[Int]
  def set(address: Address, heights: RemoteData[Seq[Int]])(implicit ctx: ReadOnly): Unit
  def get(address: Address)(implicit ctx: ReadOnly): RemoteData[Seq[Int]]
  def removeFrom(height: Height)(implicit ctx: ReadWrite): List[Address]
}
