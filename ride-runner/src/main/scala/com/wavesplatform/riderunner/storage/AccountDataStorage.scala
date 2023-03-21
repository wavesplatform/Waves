package com.wavesplatform.riderunner.storage

import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaDataEntry
import com.wavesplatform.riderunner.storage.persistent.PersistentCache
import com.wavesplatform.state.DataEntry

// TODO #32 Not all addresses are interesting. Probably, having a wrapped Map[Address, Map[Asset, Long]] is better, because we can filter out values slightly before.
class AccountDataStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    chainId: Byte,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[AccountDataKey, DataEntry[?]]
) extends ExactWithHeightStorage[AccountDataKey, DataEntry[?], TagT] {
  override def getFromBlockchain(key: AccountDataKey): Option[DataEntry[?]] = blockchainApi.getAccountDataEntry(key._1, key._2)

  def append(height: Int, update: StateUpdate.DataEntryUpdate): AffectedTags[TagT] =
    append(height, (toVanillaAddress(update.address, chainId), update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

  def undoAppend(height: Int, update: StateUpdate.DataEntryUpdate): AffectedTags[TagT] =
    undoAppend(height, (toVanillaAddress(update.address, chainId), update.getDataEntry.key))

  def rollback(height: Int, update: StateUpdate.DataEntryUpdate): AffectedTags[TagT] =
    rollback(height, (toVanillaAddress(update.address, chainId), update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

}
