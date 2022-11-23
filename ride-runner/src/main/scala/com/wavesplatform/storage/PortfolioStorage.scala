package com.wavesplatform.storage

import cats.syntax.option.*
import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

class PortfolioStorage[TagT](blockchainApi: BlockchainGrpcApi, override val persistentCache: PersistentCache[Address, Portfolio])
    extends HeightStorage[Address, Portfolio, TagT] {
  override def getFromBlockchain(key: Address): Option[Portfolio] = blockchainApi.getBalances(key).some

  def append(height: Int, update: StateUpdate.BalanceUpdate): AppendResult[TagT] = {
    val address = update.address.toAddress
    memoryCache.get(address) match {
      case None => AppendResult.ignored
      case Some(origData) =>
        val (asset, after) = toAssetAndAmount(update.getAmountAfter)

        val orig = origData.data.mayBeValue.getOrElse(Portfolio.empty)
        val updated =
          if (after == 0 && orig.isEmpty) none
          else
            Some(asset match {
              case Asset.Waves        => orig.copy(balance = after)
              case asset: IssuedAsset => orig.copy(assets = orig.assets.updated(asset, after))
            })

        super.append(height, address, updated) // TODO suboptimal, probably separate balances and leasing?
    }
  }

  def append(height: Int, update: StateUpdate.LeasingUpdate): AppendResult[TagT] = {
    val address = update.address.toAddress
    memoryCache.get(address) match {
      case None => AppendResult.ignored
      case Some(origData) =>
        val updatedLease = LeaseBalance(update.inAfter, update.outAfter)

        val orig = origData.data.mayBeValue.getOrElse(Portfolio.empty)
        val updated =
          if (updatedLease.out == 0 && orig.isEmpty) none
          else orig.copy(lease = updatedLease).some

        append(height, address, updated) // TODO suboptimal
    }
  }

  // TODO not optimal, because we need to access DB again and again for each asset and leasing.
  def rollback(rollbackHeight: Int, update: StateUpdate.BalanceUpdate): RollbackResult[TagT] = {
    val address = update.address.toAddress
    memoryCache.get(address) match {
      case None => RollbackResult.ignored
      case Some(origData) =>
        val (asset, after) = toAssetAndAmount(update.getAmountAfter)

        val orig = origData.data.mayBeValue.getOrElse(Portfolio.empty)
        val updated =
          if (after == 0 && orig.isEmpty) none
          else
            Some(asset match {
              case Asset.Waves        => orig.copy(balance = after)
              case asset: IssuedAsset => orig.copy(assets = orig.assets.updated(asset, after))
            })

        super.rollback(rollbackHeight, update.address.toAddress, updated)
    }
  }

  def rollback(rollbackHeight: Int, update: StateUpdate.LeasingUpdate): RollbackResult[TagT] = {
    val address = update.address.toAddress
    memoryCache.get(address) match {
      case None           => RollbackResult.ignored
      case Some(origData) =>
        // TODO copy-paste from append
        val updatedLease = LeaseBalance(update.inAfter, update.outAfter)

        val orig = origData.data.mayBeValue.getOrElse(Portfolio.empty)
        val updated =
          if (updatedLease.out == 0 && orig.isEmpty) none
          else orig.copy(lease = updatedLease).some

        super.rollback(rollbackHeight, update.address.toAddress, updated)
    }
  }
}
