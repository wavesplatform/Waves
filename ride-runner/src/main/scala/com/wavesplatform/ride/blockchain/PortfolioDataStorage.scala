package com.wavesplatform.ride.blockchain

import cats.syntax.option.*
import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.ride.blockchain.DataKey.PortfolioDataKey
import com.wavesplatform.ride.blockchain.caches.BlockchainCaches
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

class PortfolioDataStorage[TagT](caches: BlockchainCaches, blockchainApi: BlockchainGrpcApi) extends DataStorage[Address, Portfolio, TagT] {
  override def mkDataKey(key: Address): DataKey = PortfolioDataKey(key)

  override def getFromBlockchain(key: Address): Option[Portfolio] = blockchainApi.getBalances(key).some

  override def getFromPersistentCache(maxHeight: Int, key: Address): BlockchainData[Portfolio] = caches.getBalances(key, maxHeight)

  override def setPersistentCache(height: Int, key: Address, data: BlockchainData[Portfolio]): Unit =
    caches.setBalances(key, height, data)

  override def removeFromPersistentCache(fromHeight: Int, key: Address): BlockchainData[Portfolio] =
    caches.removeBalances(key, fromHeight)

  def append(height: Int, update: StateUpdate.BalanceUpdate): AppendResult[TagT] = {
    val address = update.address.toAddress
    memoryCache.get(address) match {
      case None => AppendResult.ignored
      case Some(origData) =>
        val (asset, after) = toAssetAndAmount(update.getAmountAfter)
        log.debug(s"Updated balance($asset, $after)")

        val orig = origData.data.mayBeValue.getOrElse(Portfolio.empty)
        val updated = asset match {
          case Asset.Waves        => orig.copy(balance = after)
          case asset: IssuedAsset => orig.copy(assets = orig.assets.updated(asset, after))
        }

        super.append(height, address, updated.some) // TODO suboptimal, probably separate balances and leasing?
    }
  }

  def append(height: Int, update: StateUpdate.LeasingUpdate): AppendResult[TagT] = {
    val address = update.address.toAddress
    memoryCache.get(address) match {
      case None => AppendResult.ignored
      case Some(origData) =>
        val updatedLease = LeaseBalance(update.inAfter, update.outAfter)
        log.debug(s"Updated leasing($address)")

        val orig    = origData.data.mayBeValue.getOrElse(Portfolio.empty)
        val updated = orig.copy(lease = updatedLease)

        append(height, address, updated.some) // TODO suboptimal
    }
  }
}
