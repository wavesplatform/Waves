package scorex

import com.wavesplatform.utils.base58Length
import scorex.block.{Block, MicroBlock}
import scorex.crypto.hash.FastCryptographicHash

package object transaction {

  type AssetId = com.wavesplatform.state2.ByteStr
  val AssetIdLength = FastCryptographicHash.DigestSize
  val AssetIdStringLength = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks = Seq[Block]
  type DiscardedMicroBlocks = Seq[MicroBlock]

}
