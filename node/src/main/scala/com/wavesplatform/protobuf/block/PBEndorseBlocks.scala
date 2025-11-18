package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*
import com.wavesplatform.state.{GeneratorIndex, Height}

object PBEndorseBlocks {
  def vanilla(x: PBEndorseBlock, sig: BlsSignature.NonEmpty): BlockEndorsement = BlockEndorsement(
    GeneratorIndex(x.endorserIndex),
    x.finalizedBlockId.toByteStr,
    Height(x.finalizedBlockHeight),
    x.endorsedBlockId.toByteStr,
    sig
  )

  def protobuf(x: BlockEndorsement): PBEndorseBlock = new PBEndorseBlock(
    endorserIndex = x.endorserIndex.toInt,
    finalizedBlockId = x.finalizedId.toByteString,
    finalizedBlockHeight = x.finalizedHeight,
    endorsedBlockId = x.endorsedId.toByteString,
    signature = x.signature.byteStr.toByteString
  )
}
