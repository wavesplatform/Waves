package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.account.Address
import com.wavesplatform.ride.runner.storage.*
import com.wavesplatform.state.{DataEntry, Height, LeaseBalance}
import com.wavesplatform.transaction.Asset

trait PersistentCaches {
  def accountDataEntries: PersistentCache[CacheKey.AccountData, DataEntry[?]]
  def accountScripts: PersistentCache[Address, WeighedAccountScriptInfo]
  def assetDescriptions: PersistentCache[Asset.IssuedAsset, WeighedAssetDescription]
  def aliases: AliasPersistentCache
  def accountBalances: PersistentCache[AccountAssetKey, Long]
  def accountLeaseBalances: PersistentCache[Address, LeaseBalance]
  def transactions: TransactionPersistentCache
  def blockHeaders: BlockPersistentCache
  def addressIds: AddressIdPersistentCache

  def getActivatedFeatures(): RemoteData[Map[Short, Height]]
  def setActivatedFeatures(data: Map[Short, Height]): Unit
}
