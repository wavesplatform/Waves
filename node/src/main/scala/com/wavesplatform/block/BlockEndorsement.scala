package com.wavesplatform.block

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey, BlsSignature}
import com.wavesplatform.state.Height

enum BlockEndorsement {
  case Full(endorser: BlsPublicKey, finalizedBlockId: BlockId, blockId: BlockId, blockHeight: Height, signature: BlsSignature)
  case Conflict(endorser: BlsPublicKey, finalizedBlockId: BlockId, blockId: BlockId, signature: BlsSignature)
  case Valid(endorser: BlsPublicKey, finalizedBlockId: BlockId, signature: BlsSignature)
}

object BlockEndorsement {
  def sign(kp: BlsKeyPair, finalizedId: BlockId, id: BlockId, height: Height): BlsSignature =
    kp.sign(mkMessage(finalizedId, id, height))

  def mkMessage(finalizedId: BlockId, id: BlockId, height: Height): Array[Byte] = finalizedId.arr ++ id.arr ++ Ints.toByteArray(height)

  def full(account: BlsKeyPair, finalizedId: BlockId, id: BlockId, height: Height): BlockEndorsement.Full =
    BlockEndorsement.Full(account.publicKey, finalizedId, id, height, sign(account, finalizedId, id, height))
}
