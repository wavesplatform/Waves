package com.wavesplatform.storage

import com.wavesplatform.account.Address
import com.wavesplatform.blockchain.*
import com.wavesplatform.blockchain.caches.PersistentCache
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaDataEntry
import com.wavesplatform.state.DataEntry
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}

class AccountDataStorage[TagT](blockchainApi: BlockchainGrpcApi, override val persistentCache: PersistentCache[AccountDataKey, DataEntry[?]])
    extends Storage[AccountDataKey, DataEntry[?], TagT] { storage =>
  override def mkDataKey(key: AccountDataKey): DataKey = AccountDataDataKey(key._1, key._2)

  override def getFromBlockchain(key: AccountDataKey): Option[DataEntry[?]] = blockchainApi.getAccountDataEntry(key._1, key._2)

  def append(height: Int, update: StateUpdate.DataEntryUpdate): AppendResult[TagT] =
    append(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

  def rollback(height: Int, update: StateUpdate.DataEntryUpdate): RollbackResult[TagT] =
    rollback(height, (update.address.toAddress, update.getDataEntry.key), update.dataEntry.map(toVanillaDataEntry))

  private case class AccountDataDataKey(address: Address, key: String) extends DataKey {
    override def reload(height: Int): Unit = storage.reload(height, (address, key))
  }
}
