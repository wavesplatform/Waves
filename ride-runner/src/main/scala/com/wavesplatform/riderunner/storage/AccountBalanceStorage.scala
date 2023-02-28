package com.wavesplatform.riderunner.storage

import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.PersistentCache
import com.wavesplatform.state.Height

// TODO #32 Not all addresses are interesting. Probably, having a wrapped Map[Address, Map[Asset, Long]] is better, because we can filter out values slightly before.
class AccountBalanceStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    chainId: Byte,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[AccountAssetKey, Long]
) extends ExactWithHeightStorage[AccountAssetKey, Long, TagT] {
  override def getFromBlockchain(key: AccountAssetKey): Option[Long] = Option(blockchainApi.getBalance(key._1, key._2))

  def append(atHeight: Height, update: StateUpdate.BalanceUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (key, after) = getKeyAndAfter(update)
    append(atHeight, key, after)
  }

  def undoAppend(toHeight: Height, update: StateUpdate.BalanceUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (key, _) = getKeyAndAfter(update)
    undoAppend(toHeight, key)
  }

  def rollbackTo(toHeight: Height, update: StateUpdate.BalanceUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (key, after) = getKeyAndAfter(update)
    rollback(toHeight, key, after)
  }

  private def getKeyAndAfter(update: StateUpdate.BalanceUpdate): (AccountAssetKey, Long) = {
    val address        = toVanillaAddress(update.address, chainId)
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    ((address, asset), after)
  }
}
