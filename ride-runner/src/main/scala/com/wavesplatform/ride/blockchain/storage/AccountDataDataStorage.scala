package com.wavesplatform.ride.blockchain.storage

import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaDataEntry
import com.wavesplatform.ride.blockchain.*
import com.wavesplatform.ride.blockchain.DataKey.AccountDataDataKey
import com.wavesplatform.ride.blockchain.caches.PersistentCache
import com.wavesplatform.state.DataEntry

class AccountDataDataStorage[TagT](blockchainApi: BlockchainGrpcApi, override val persistentCache: PersistentCache[AccountDataKey, DataEntry[?]])
    extends DataStorage[AccountDataKey, DataEntry[?], TagT] {
  override def mkDataKey(key: AccountDataKey): DataKey = AccountDataDataKey(key._1, key._2)

  override def getFromBlockchain(key: AccountDataKey): Option[DataEntry[?]] = blockchainApi.getAccountDataEntry(key._1, key._2)

  def append(height: Int, update: StateUpdate.DataEntryUpdate): AppendResult[TagT] =
    append(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

  def rollback(height: Int, update: StateUpdate.DataEntryUpdate): RollbackResult[TagT] = {
    // TODO copy-paste from append
    rollback(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))
  }
}
