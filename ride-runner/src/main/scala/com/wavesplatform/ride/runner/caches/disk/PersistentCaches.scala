package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.*
import com.wavesplatform.state.{DataEntry, Height, LeaseBalance}

trait PersistentCaches {
  def blockHeaders: BlockPersistentCache

  def accountDataEntries: PersistentCache[CacheKey.AccountData, DataEntry[?]]
  def accountScripts: PersistentCache[CacheKey.AccountScript, WeighedAccountScriptInfo]
  def assetDescriptions: PersistentCache[CacheKey.Asset, WeighedAssetDescription]
  def aliases: AliasPersistentCache
  def accountBalances: PersistentCache[CacheKey.AccountBalance, Long]
  def accountLeaseBalances: PersistentCache[CacheKey.AccountLeaseBalance, LeaseBalance]
  def transactions: TransactionPersistentCache

  def addressIds: AddressIdPersistentCache

  def getActivatedFeatures(): RemoteData[Map[Short, Height]]
  def setActivatedFeatures(data: Map[Short, Height]): Unit
}
