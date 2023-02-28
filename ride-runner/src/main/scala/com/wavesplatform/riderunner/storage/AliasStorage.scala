package com.wavesplatform.riderunner.storage

import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.PersistentCache
import com.wavesplatform.state.Height

// It seems, we don't need to update this. Only for some optimization needs
// TODO #74 Do we need a height for aliases ?
class AliasStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    chainId: Byte,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[Alias, Address]
) extends ExactWithHeightStorage[Alias, Address, TagT] {
  override def getFromBlockchain(key: Alias): Option[Address] = blockchainApi.resolveAlias(key)

  def append(atHeight: Height, name: String, account: PublicKey)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    append(atHeight, mkAlias(name), account.toAddress(chainId))

  def undoAppend(toHeight: Height, name: String)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    undoAppend(toHeight, mkAlias(name))

  private def mkAlias(name: String): Alias = Alias.createWithChainId(name, chainId).explicitGet() // Can't fail, because receive verified
}
