package com.wavesplatform.ride.runner.storage

import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.ride.runner.storage.StorageContext.ReadWrite
import com.wavesplatform.ride.runner.storage.persistent.PersistentCache
import com.wavesplatform.state.{Height, LeaseBalance}

class AccountLeaseBalanceStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    chainId: Byte,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[Address, LeaseBalance]
) extends ExactWithHeightStorage[Address, LeaseBalance, TagT] {
  override def getFromBlockchain(key: Address): Option[LeaseBalance] = {
    val r = blockchainApi.getLeaseBalance(key)
    Some(LeaseBalance(r.leaseIn, r.leaseOut))
  }

  def append(atHeight: Height, update: StateUpdate.LeasingUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val address = toVanillaAddress(update.address, chainId)
    append(atHeight, address, toVanilla(update))
  }

  def undoAppend(toHeight: Height, update: StateUpdate.LeasingUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    undoAppend(toHeight, update.address.toAddress)

  // TODO #21 Copy-paste from append
  def rollbackTo(toHeight: Height, update: StateUpdate.LeasingUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val address = toVanillaAddress(update.address, chainId)
    rollback(toHeight, address, toVanilla(update))
  }

  private def toVanilla(x: StateUpdate.LeasingUpdate): LeaseBalance = LeaseBalance(x.inAfter, x.outAfter)
}
