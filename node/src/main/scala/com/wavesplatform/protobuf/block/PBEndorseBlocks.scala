package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*
import com.wavesplatform.state.Height

object PBEndorseBlocks {
  // TODO:
  def vanilla(x: PBEndorseBlock, sig: BlsSignature.NonEmpty): BlockEndorsement = {
    if (x.blockId.isEmpty && x.blockHeight == 0)
      BlockEndorsement.Valid(x.endorserPublicKey.toBlsPublicKey, x.finalizedBlockId.toByteStr, sig)
    else if (x.blockHeight == 0)
      BlockEndorsement.Conflict(
        x.endorserPublicKey.toBlsPublicKey,
        x.finalizedBlockId.toByteStr,
        x.blockId.toByteStr,
        sig
      )
    else
      BlockEndorsement.Full(
        x.endorserPublicKey.toBlsPublicKey,
        x.finalizedBlockId.toByteStr,
        x.blockId.toByteStr,
        Height(x.blockHeight),
        sig
      )
  }

  def protobuf(x: BlockEndorsement.Full): PBEndorseBlock =
    new PBEndorseBlock(
      endorserPublicKey = x.endorser.byteStr.toByteString,
      finalizedBlockId = x.finalizedBlockId.toByteString,
      blockId = x.blockId.toByteString,
      blockHeight = x.blockHeight,
      signature = x.signature.byteStr.toByteString
    )
}
