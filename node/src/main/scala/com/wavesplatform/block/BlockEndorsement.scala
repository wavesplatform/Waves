package com.wavesplatform.block

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Height

enum BlockEndorsement {
  case Full(endorser: BlsPublicKey, finalizedBlockId: BlockId, blockId: BlockId, blockHeight: Height, signature: ByteStr)
  case Conflict(endorser: BlsPublicKey, finalizedBlockId: BlockId, blockId: BlockId, signature: ByteStr)
  case Valid(endorser: BlsPublicKey, finalizedBlockId: BlockId, signature: ByteStr)
}

object BlockEndorsement {
  def sign(kp: BlsKeyPair, finalizedId: BlockId, id: BlockId, height: Height): ByteStr =
    ByteStr(kp.sign(mkMessage(finalizedId, id, height)))

  def mkMessage(finalizedId: BlockId, id: BlockId, height: Height): Array[Byte] = finalizedId.arr ++ id.arr ++ Ints.toByteArray(height)

  def full(account: BlsKeyPair, finalizedId: BlockId, id: BlockId, height: Height): BlockEndorsement.Full =
    BlockEndorsement.Full(account.publicKey, finalizedId, id, height, sign(account, finalizedId, id, height))
}
