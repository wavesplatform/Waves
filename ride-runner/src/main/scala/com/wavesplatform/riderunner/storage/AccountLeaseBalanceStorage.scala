package com.wavesplatform.riderunner.storage

import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.riderunner.storage.actions.AffectedTags
import com.wavesplatform.riderunner.storage.persistent.PersistentCache
import com.wavesplatform.state.LeaseBalance

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

  def append(height: Int, update: StateUpdate.LeasingUpdate): AffectedTags[TagT] = {
    val address = toVanillaAddress(update.address, chainId)
    append(height, address, toVanilla(update))
  }

  def undoAppend(height: Int, update: StateUpdate.LeasingUpdate): AffectedTags[TagT] = undoAppend(height, update.address.toAddress)

  // TODO #21 Copy-paste from append
  def rollback(rollbackHeight: Int, update: StateUpdate.LeasingUpdate): AffectedTags[TagT] = {
    val address = toVanillaAddress(update.address, chainId)
    rollback(rollbackHeight, address, toVanilla(update))
  }

  private def toVanilla(x: StateUpdate.LeasingUpdate): LeaseBalance = LeaseBalance(x.inAfter, x.outAfter)
}
