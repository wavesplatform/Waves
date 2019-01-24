package com.wavesplatform

import com.wavesplatform.utils.base58Length
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr

package object transaction {

  type AssetId = ByteStr
  val AssetIdLength: Int       = com.wavesplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
