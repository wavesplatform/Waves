package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*
import com.wavesplatform.state.Height

object PBEndorseBlocks {
  // TODO:
  def vanilla(x: PBEndorseBlock, sig: BlsSignature.NonEmpty): BlockEndorsement = {
    if (x.endorsedBlockId.isEmpty && x.finalizedBlockHeight == 0)
      BlockEndorsement.Consistent(x.endorserIndex, x.finalizedBlockId.toByteStr, sig)
    else if (x.finalizedBlockHeight == 0)
      BlockEndorsement.Conflict(
        x.endorserIndex,
        x.finalizedBlockId.toByteStr,
        sig
      )
    else
      BlockEndorsement.Full(
        x.endorserIndex,
        x.finalizedBlockId.toByteStr,
        Height(x.finalizedBlockHeight),
        x.endorsedBlockId.toByteStr,
        signature = sig
      )
  }

  def protobuf(x: BlockEndorsement.Full): PBEndorseBlock =
    new PBEndorseBlock(
      endorserIndex = x.endorserIndex,
      finalizedBlockId = x.finalizedId.toByteString,
      finalizedBlockHeight = x.finalizedHeight,
      endorsedBlockId = x.endorsedId.toByteString,
      signature = x.signature.byteStr.toByteString
    )
}
