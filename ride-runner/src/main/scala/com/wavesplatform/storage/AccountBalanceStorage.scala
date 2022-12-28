package com.wavesplatform.storage

import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache

// TODO #32 Not all addresses are interesting. Probably, having a wrapped Map[Address, Map[Asset, Long]] is better, because we can filter out values slightly before.
class AccountBalanceStorage[TagT](blockchainApi: BlockchainApi, override val persistentCache: PersistentCache[AccountAssetKey, Long])
    extends ExactWithHeightStorage[AccountAssetKey, Long, TagT] {
  override def getFromBlockchain(key: AccountAssetKey): Option[Long] = Option(blockchainApi.getBalance(key._1, key._2))

  def append(height: Int, update: StateUpdate.BalanceUpdate): AppendResult[TagT] = {
    val address        = update.address.toAddress
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    append(height, (address, asset), after)
  }

  def undoAppend(height: Int, update: StateUpdate.BalanceUpdate): RollbackResult[TagT] = {
    val address    = update.address.toAddress
    val (asset, _) = toAssetAndAmount(update.getAmountAfter)
    undoAppend(height, (address, asset))
  }

  def rollback(rollbackHeight: Int, update: StateUpdate.BalanceUpdate): RollbackResult[TagT] = {
    val address        = update.address.toAddress
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    rollback(rollbackHeight, (address, asset), after)
  }
}
