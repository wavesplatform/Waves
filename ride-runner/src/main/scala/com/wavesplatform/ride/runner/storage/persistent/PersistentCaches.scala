package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.ride.runner.storage.{AccountAssetKey, AccountDataKey, RemoteData}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Height, LeaseBalance}
import com.wavesplatform.transaction.Asset

trait PersistentCaches {
  def accountDataEntries: PersistentCache[AccountDataKey, DataEntry[?]]
  def accountScripts: PersistentCache[Address, AccountScriptInfo]
  def assetDescriptions: PersistentCache[Asset.IssuedAsset, AssetDescription]
  def aliases: PersistentCache[Alias, Address]
  def accountBalances: PersistentCache[AccountAssetKey, Long]
  def accountLeaseBalances: PersistentCache[Address, LeaseBalance]
  def transactions: TransactionPersistentCache
  def blockHeaders: BlockPersistentCache
  def addressIds: AddressIdPersistentCache

  def getActivatedFeatures(): RemoteData[Map[Short, Height]]
  def setActivatedFeatures(data: Map[Short, Height]): Unit
}
