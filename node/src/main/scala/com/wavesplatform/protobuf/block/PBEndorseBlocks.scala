package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*

object PBEndorseBlocks {
  def vanillaConflict(x: PBEndorseBlock, sig: BlsSignature.NonEmpty): Either[String, BlockEndorsement.Conflict] =
    Either.cond(
      x.endorsedBlockId.isEmpty && x.finalizedBlockHeight == 0,
      BlockEndorsement.Conflict(
        x.endorserIndex,
        x.finalizedBlockId.toByteStr,
        sig
      ),
      "Expected a conflict endorsement"
    )

  def protobuf(x: BlockEndorsement.Full): PBEndorseBlock =
    new PBEndorseBlock(
      endorserIndex = x.endorserIndex,
      finalizedBlockId = x.finalizedId.toByteString,
      finalizedBlockHeight = x.finalizedHeight,
      endorsedBlockId = x.endorsedId.toByteString,
      signature = x.signature.byteStr.toByteString
    )
}
