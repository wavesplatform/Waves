package com.wavesplatform.storage

import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.storage.persistent.PersistentCache

// TODO #32 Not all addresses are interesting. Probably, having a wrapped Map[Address, Map[Asset, Long]] is better, because we can filter out values slightly before.
class AccountBalanceStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    chainId: Byte,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[AccountAssetKey, Long]
) extends ExactWithHeightStorage[AccountAssetKey, Long, TagT] {
  override def getFromBlockchain(key: AccountAssetKey): Option[Long] = Option(blockchainApi.getBalance(key._1, key._2))

  def append(height: Int, update: StateUpdate.BalanceUpdate): AffectedTags[TagT] = {
    val (key, after) = getKeyAndAfter(update)
    append(height, key, after)
  }

  def undoAppend(height: Int, update: StateUpdate.BalanceUpdate): AffectedTags[TagT] = {
    val (key, _) = getKeyAndAfter(update)
    undoAppend(height, key)
  }

  def rollback(rollbackHeight: Int, update: StateUpdate.BalanceUpdate): AffectedTags[TagT] = {
    val (key, after) = getKeyAndAfter(update)
    rollback(rollbackHeight, key, after)
  }

  private def getKeyAndAfter(update: StateUpdate.BalanceUpdate): (AccountAssetKey, Long) = {
    val address        = toVanillaAddress(update.address, chainId)
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    ((address, asset), after)
  }
}
