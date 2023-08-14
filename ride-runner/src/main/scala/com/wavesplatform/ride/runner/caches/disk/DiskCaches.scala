package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.*
import com.wavesplatform.ride.runner.caches.mem.MemCacheKey
import com.wavesplatform.state.{DataEntry, Height, LeaseBalance}

trait DiskCaches {
  def blockHeaders: BlockDiskCache

  def accountDataEntries: DiskCache[MemCacheKey.AccountData, DataEntry[?]]
  def accountScripts: DiskCache[MemCacheKey.AccountScript, WeighedAccountScriptInfo]
  def assetDescriptions: DiskCache[MemCacheKey.Asset, WeighedAssetDescription]
  def aliases: AliasDiskCache
  def accountBalances: DiskCache[MemCacheKey.AccountBalance, Long]
  def accountLeaseBalances: DiskCache[MemCacheKey.AccountLeaseBalance, LeaseBalance]
  def transactions: TransactionDiskCache

  def addressIds: AddressIdDiskCache

  def getActivatedFeatures(): RemoteData[Map[Short, Height]]
  def setActivatedFeatures(data: Map[Short, Height]): Unit
}
