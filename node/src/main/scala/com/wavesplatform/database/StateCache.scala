package com.wavesplatform.database

/** The Guava state caches whose statistics `Caches.cacheStats` reports. */
enum StateCache {
  case Balance, LeaseBalance, AssetDescription, VolumeAndFee, AccountScript, AssetScript, AddressId, AccountData, Alias, BlockHeight,
    BalanceAtHeight, LeaseBalanceAtHeight, BalanceSnapshots
}
