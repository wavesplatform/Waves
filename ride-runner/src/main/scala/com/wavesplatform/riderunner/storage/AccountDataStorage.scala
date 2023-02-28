package com.wavesplatform.riderunner.storage

import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaDataEntry
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.PersistentCache
import com.wavesplatform.state.{DataEntry, Height}

// TODO #32 Not all addresses are interesting. Probably, having a wrapped Map[Address, Map[Asset, Long]] is better, because we can filter out values slightly before.
class AccountDataStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    chainId: Byte,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[AccountDataKey, DataEntry[?]]
) extends ExactWithHeightStorage[AccountDataKey, DataEntry[?], TagT] {
  override def getFromBlockchain(key: AccountDataKey): Option[DataEntry[?]] = blockchainApi.getAccountDataEntry(key._1, key._2)

  def append(atHeight: Height, update: StateUpdate.DataEntryUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    append(atHeight, (toVanillaAddress(update.address, chainId), update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

  def undoAppend(toHeight: Height, update: StateUpdate.DataEntryUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    undoAppend(toHeight, (toVanillaAddress(update.address, chainId), update.getDataEntry.key))

  def rollbackTo(toHeight: Height, update: StateUpdate.DataEntryUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    rollback(toHeight, (toVanillaAddress(update.address, chainId), update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

}
