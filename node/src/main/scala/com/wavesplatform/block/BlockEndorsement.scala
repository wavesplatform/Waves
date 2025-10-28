package com.wavesplatform.block

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.state.Height

enum BlockEndorsement {
  case Full(endorserIndex: Int, finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId, signature: BlsSignature.NonEmpty)
  case Conflict(endorserIndex: Int, finalizedId: BlockId, signature: BlsSignature.NonEmpty)
}

object BlockEndorsement {
  def sign(kp: BlsKeyPair, finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): BlsSignature.NonEmpty =
    kp.sign(mkMessage(finalizedId, finalizedHeight, endorsedId))

  def mkMessage(finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): Array[Byte] =
    finalizedId.arr ++ Ints.toByteArray(finalizedHeight) ++ endorsedId.arr

  def full(
      endorserAccount: BlsKeyPair,
      endorserIndex: Int,
      finalizedId: BlockId,
      finalizedHeight: Height,
      endorsedId: BlockId
  ): BlockEndorsement.Full =
    BlockEndorsement.Full(endorserIndex, finalizedId, finalizedHeight, endorsedId, sign(endorserAccount, finalizedId, finalizedHeight, endorsedId))
}
