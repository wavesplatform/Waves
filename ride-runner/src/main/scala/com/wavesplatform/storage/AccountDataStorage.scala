package com.wavesplatform.storage

import com.wavesplatform.account.Address
import com.wavesplatform.blockchain.*
import com.wavesplatform.blockchain.caches.PersistentCache
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaDataEntry
import com.wavesplatform.state.DataEntry
import com.wavesplatform.storage.AccountDataStorage.AccountDataDataKey
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}

class AccountDataStorage[TagT](blockchainApi: BlockchainGrpcApi, override val persistentCache: PersistentCache[AccountDataKey, DataEntry[?]])
    extends Storage[AccountDataKey, DataEntry[?], TagT] {
  override def mkDataKey(key: AccountDataKey): DataKey = AccountDataDataKey(key._1, key._2)

  override def getFromBlockchain(key: AccountDataKey): Option[DataEntry[?]] = blockchainApi.getAccountDataEntry(key._1, key._2)

  def append(height: Int, update: StateUpdate.DataEntryUpdate): AppendResult[TagT] =
    append(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

  def rollback(height: Int, update: StateUpdate.DataEntryUpdate): RollbackResult[TagT] =
    rollback(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))
}

object AccountDataStorage {
  case class AccountDataDataKey(address: Address, key: String) extends DataKey {
    override type Value = DataEntry[?]

    override def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit =
      blockchainStorage.data.reload(height, (address, key))
  }
}
