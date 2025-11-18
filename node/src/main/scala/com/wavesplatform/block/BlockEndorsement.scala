package com.wavesplatform.block

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.state.{GeneratorIndex, Height}

case class BlockEndorsement(
    endorserIndex: GeneratorIndex,
    finalizedId: BlockId,
    finalizedHeight: Height,
    endorsedId: BlockId,
    signature: BlsSignature.NonEmpty
)

object BlockEndorsement {
  def signed(
      endorserAccount: BlsKeyPair,
      endorserIndex: GeneratorIndex,
      finalizedId: BlockId,
      finalizedHeight: Height,
      endorsedId: BlockId
  ): BlockEndorsement =
    BlockEndorsement(endorserIndex, finalizedId, finalizedHeight, endorsedId, sign(endorserAccount, finalizedId, finalizedHeight, endorsedId))

  def sign(kp: BlsKeyPair, finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): BlsSignature.NonEmpty =
    kp.sign(mkMessage(finalizedId, finalizedHeight, endorsedId))

  def mkMessage(finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): Array[Byte] =
    finalizedId.arr ++ Ints.toByteArray(finalizedHeight) ++ endorsedId.arr
}
