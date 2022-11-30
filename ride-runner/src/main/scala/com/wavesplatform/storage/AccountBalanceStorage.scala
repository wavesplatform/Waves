package com.wavesplatform.storage

import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache

// TODO #? Not all addresses are interesting, probably having a wrapped Map[Address, Map[Asset, Long]] is better.
// TODO #? Also see AccountDataKey
class AccountBalanceStorage[TagT](blockchainApi: BlockchainApi, override val persistentCache: PersistentCache[AccountAssetKey, Long])
    extends ExactWithHeightStorage[AccountAssetKey, Long, TagT] {
  override def getFromBlockchain(key: AccountAssetKey): Option[Long] = Option(blockchainApi.getBalance(key._1, key._2))

  def append(height: Int, update: StateUpdate.BalanceUpdate): AppendResult[TagT] = {
    val address        = update.address.toAddress
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    val key            = (address, asset)
    memoryCache.get(key) match {
      case None => AppendResult.ignored
      case _    => super.append(height, key, after)
    }
  }

  def rollback(rollbackHeight: Int, update: StateUpdate.BalanceUpdate): RollbackResult[TagT] = {
    val address        = update.address.toAddress
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    val key            = (address, asset)
    memoryCache.get(key) match {
      case None => RollbackResult.ignored
      case _    => super.rollback(rollbackHeight, key, after)
    }
  }
}
