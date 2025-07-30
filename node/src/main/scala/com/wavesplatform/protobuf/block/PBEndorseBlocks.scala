package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.protobuf.*
import com.wavesplatform.state.Height

object PBEndorseBlocks {
  def vanilla(x: PBEndorseBlock): BlockEndorsement =
    if (x.blockId.isEmpty && x.blockHeight == 0)
      BlockEndorsement.Valid(x.endorserPublicKey.toBlsPublicKey, x.finalizedBlockId.toByteStr, x.signature.toByteStr)
    else if (x.blockHeight == 0)
      BlockEndorsement.Conflict(x.endorserPublicKey.toBlsPublicKey, x.finalizedBlockId.toByteStr, x.blockId.toByteStr, x.signature.toByteStr)
    else
      BlockEndorsement.Full(
        x.endorserPublicKey.toBlsPublicKey,
        x.finalizedBlockId.toByteStr,
        x.blockId.toByteStr,
        Height(x.blockHeight),
        x.signature.toByteStr
      )

  def protobuf(x: BlockEndorsement.Full): PBEndorseBlock =
    new PBEndorseBlock(
      endorserPublicKey = x.endorser.asByteStr.toByteString,
      finalizedBlockId = x.finalizedBlockId.toByteString,
      blockId = x.blockId.toByteString,
      blockHeight = x.blockHeight,
      signature = x.signature.toByteString
    )
}
