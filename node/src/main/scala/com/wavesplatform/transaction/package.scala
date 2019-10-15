package com.wavesplatform

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.utils.base58Length
import io.estatico.newtype.macros.newtype

package object transaction {
  val AssetIdLength: Int       = com.wavesplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction

  @newtype case class TransactionVersion(toByte: Byte)
  @newtype case class TransactionType(toByte: Byte)
}
