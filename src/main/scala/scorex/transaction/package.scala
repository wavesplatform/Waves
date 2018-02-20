package scorex

import com.wavesplatform.utils.base58Length
import scorex.block.{Block, MicroBlock}

package object transaction {

  type AssetId = com.wavesplatform.state2.ByteStr
  val AssetIdLength: Int = com.wavesplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks = Seq[Block]
  type DiscardedMicroBlocks = Seq[MicroBlock]

}
