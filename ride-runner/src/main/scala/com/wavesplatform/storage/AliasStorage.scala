package com.wavesplatform.storage

import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.storage.persistent.PersistentCache

// It seems, we don't need to update this. Only for some optimization needs
class AliasStorage[TagT](chainId: Byte, blockchainApi: BlockchainApi, override val persistentCache: PersistentCache[Alias, Address])
    extends ExactWithHeightStorage[Alias, Address, TagT] {
  override def getFromBlockchain(key: Alias): Option[Address] = blockchainApi.resolveAlias(key)

  def append(height: Int, rawAlias: String, account: PublicKey): AffectedTags[TagT] =
    append(height, Alias.createWithChainId(rawAlias, chainId).explicitGet(), account.toAddress(chainId))

  def undoAppend(height: Int, rawAlias: String): AffectedTags[TagT] =
    undoAppend(height, Alias.createWithChainId(rawAlias, chainId).explicitGet())
}
