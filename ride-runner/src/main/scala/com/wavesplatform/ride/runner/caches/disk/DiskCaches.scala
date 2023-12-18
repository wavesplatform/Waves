package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.ride.runner.caches.*
import com.wavesplatform.state.{DataEntry, Height, LeaseBalance}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

trait DiskCaches {
  def blockHeaders: BlockDiskCache
  def maliciousMiners: MaliciousMinerBanHeightsDiskCache

  def accountDataEntries: DiskCache[(Address, String), DataEntry[?]]
  def accountScripts: DiskCache[Address, WeighedAccountScriptInfo]
  def assetDescriptions: DiskCache[IssuedAsset, WeighedAssetDescription]
  def aliases: AliasDiskCache
  def accountBalances: DiskCache[(Address, Asset), Long]
  def accountLeaseBalances: DiskCache[Address, LeaseBalance]
  def transactions: TransactionDiskCache

  def addressIds: AddressIdDiskCache

  def getActivatedFeatures(): RemoteData[Map[Short, Height]]
  def setActivatedFeatures(data: Map[Short, Height]): Unit
}
