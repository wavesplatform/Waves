package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.*
import com.wavesplatform.state.{DataEntry, Height, LeaseBalance}

trait DiskCaches {
  def blockHeaders: BlockDiskCache

  def accountDataEntries: DiskCache[CacheKey.AccountData, DataEntry[?]]
  def accountScripts: DiskCache[CacheKey.AccountScript, WeighedAccountScriptInfo]
  def assetDescriptions: DiskCache[CacheKey.Asset, WeighedAssetDescription]
  def aliases: AliasDiskCache
  def accountBalances: DiskCache[CacheKey.AccountBalance, Long]
  def accountLeaseBalances: DiskCache[CacheKey.AccountLeaseBalance, LeaseBalance]
  def transactions: TransactionDiskCache

  def addressIds: AddressIdDiskCache

  def getActivatedFeatures(): RemoteData[Map[Short, Height]]
  def setActivatedFeatures(data: Map[Short, Height]): Unit
}
