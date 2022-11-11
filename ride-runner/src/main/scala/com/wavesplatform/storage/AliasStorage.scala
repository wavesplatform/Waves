package com.wavesplatform.storage

import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.storage.actions.AppendResult
import com.wavesplatform.storage.persistent.PersistentCache

// It seems, we don't need to update this. Only for some optimization needs
class AliasStorage[TagT](chainId: Byte, blockchainApi: BlockchainGrpcApi, override val persistentCache: PersistentCache[Alias, Address])
    extends HeightStorage[Alias, Address, TagT]
    with HasAnyRefMap[Alias, Address, TagT] {
  override def getFromBlockchain(key: Alias): Option[Address] = blockchainApi.resolveAlias(key)

  def append(height: Int, rawAlias: String, account: PublicKey): AppendResult[TagT] =
    append(height, Alias.fromString(rawAlias).explicitGet(), Some(account.toAddress(chainId)))
}
