package com.wavesplatform.block

import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Height

enum BlockEndorsement {
  case NetworkMessage(endorser: PublicKey, blockId: BlockId, blockHeight: Height, signature: ByteStr)
  case Valid(endorser: PublicKey, signature: ByteStr)
  case Alternative(endorser: PublicKey, signature: ByteStr, referenceHash: ByteStr)
}
