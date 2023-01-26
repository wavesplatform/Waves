package com.wavesplatform.storage

import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.storage.persistent.PersistentCache

// It seems, we don't need to update this. Only for some optimization needs
// TODO #74 Do we need a height for aliases ?
class AliasStorage[TagT](chainId: Byte, blockchainApi: BlockchainApi, override val persistentCache: PersistentCache[Alias, Address])
    extends ExactWithHeightStorage[Alias, Address, TagT] {
  override def getFromBlockchain(key: Alias): Option[Address] = blockchainApi.resolveAlias(key)

  def append(height: Int, name: String, account: PublicKey): AffectedTags[TagT] = append(height, mkAlias(name), account.toAddress(chainId))
  def undoAppend(height: Int, name: String): AffectedTags[TagT]                 = undoAppend(height, mkAlias(name))

  private def mkAlias(name: String): Alias = Alias.createWithChainId(name, chainId).explicitGet() // Can't fail, because receive verified
}
