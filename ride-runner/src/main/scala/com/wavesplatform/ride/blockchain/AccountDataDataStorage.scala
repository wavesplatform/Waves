package com.wavesplatform.ride.blockchain

import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaDataEntry
import com.wavesplatform.ride.blockchain.AccountDataDataStorage.Key
import com.wavesplatform.ride.blockchain.DataKey.AccountDataDataKey
import com.wavesplatform.ride.blockchain.caches.BlockchainCaches
import com.wavesplatform.state.DataEntry

class AccountDataDataStorage[TagT](caches: BlockchainCaches, blockchainApi: BlockchainGrpcApi) extends DataStorage[Key, DataEntry[?], TagT] {
  override def mkDataKey(key: Key): DataKey = AccountDataDataKey(key._1, key._2)

  override def getFromBlockchain(key: Key): Option[DataEntry[?]] = blockchainApi.getAccountDataEntry(key._1, key._2)

  override def getFromPersistentCache(maxHeight: Int, key: Key): BlockchainData[DataEntry[?]] = caches.getAccountDataEntry(key._1, key._2, maxHeight)

  override def setPersistentCache(height: Int, key: Key, data: BlockchainData[DataEntry[?]]): Unit =
    caches.setAccountDataEntry(key._1, key._2, height, data)

  override def removeFromPersistentCache(fromHeight: Int, key: Key): BlockchainData[DataEntry[?]] =
    caches.removeAccountDataEntry(key._1, key._2, fromHeight)

  def append(height: Int, update: StateUpdate.DataEntryUpdate): AppendResult[TagT] =
    append(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

  def rollback(height: Int, update: StateUpdate.DataEntryUpdate): RollbackResult[TagT] = {
    // TODO copy-paste from append
    rollback(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))
  }
}

object AccountDataDataStorage {
  type Key = (Address, String)
}
