package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.protobuf.*
import com.wavesplatform.state.Height

object PBEndorseBlocks {
  def vanilla(x: PBEndorseBlock): BlockEndorsement =
    if (x.blockId.isEmpty && x.blockHeight == 0) BlockEndorsement.Valid(x.endorserPublicKey.toPublicKey, x.signature.toByteStr)
    else if (x.blockHeight == 0) BlockEndorsement.Alternative(x.endorserPublicKey.toPublicKey, x.signature.toByteStr, x.blockId.toByteStr)
    else BlockEndorsement.NetworkMessage(x.endorserPublicKey.toPublicKey, x.blockId.toByteStr, Height(x.blockHeight), x.signature.toByteStr)

  def protobuf(x: BlockEndorsement.NetworkMessage): PBEndorseBlock =
    new PBEndorseBlock(
      endorserPublicKey = x.endorser.toByteString,
      blockId = x.blockId.toByteString,
      blockHeight = x.blockHeight,
      signature = x.signature.toByteString
    )
}
