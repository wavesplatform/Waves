package com.wavesplatform.block

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.state.Height

enum BlockEndorsement {
  case Full(endorserIndex: Int, finalizedBlockId: BlockId, blockId: BlockId, blockHeight: Height, signature: BlsSignature.NonEmpty)
  case Conflict(endorserIndex: Int, finalizedBlockId: BlockId, blockId: BlockId, signature: BlsSignature.NonEmpty)
  case Valid(endorserIndex: Int, finalizedBlockId: BlockId, signature: BlsSignature.NonEmpty)
}

object BlockEndorsement {
  def sign(kp: BlsKeyPair, finalizedId: BlockId, id: BlockId, height: Height): BlsSignature.NonEmpty =
    kp.sign(mkMessage(finalizedId, id, height))

  def mkMessage(finalizedId: BlockId, id: BlockId, height: Height): Array[Byte] = finalizedId.arr ++ id.arr ++ Ints.toByteArray(height)

  def full(endorserAccount: BlsKeyPair, endorserIndex: Int, finalizedId: BlockId, id: BlockId, height: Height): BlockEndorsement.Full =
    BlockEndorsement.Full(endorserIndex, finalizedId, id, height, sign(endorserAccount, finalizedId, id, height))
}
