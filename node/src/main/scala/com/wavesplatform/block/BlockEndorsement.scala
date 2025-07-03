package com.wavesplatform.block

import com.google.common.primitives.Ints
import com.wavesplatform.account.{PrivateKey, PublicKey, SeedKeyPair}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.Height

enum BlockEndorsement {
  case Full(endorser: PublicKey, finalizedBlockId: BlockId, blockId: BlockId, blockHeight: Height, signature: ByteStr)
  case Conflict(endorser: PublicKey, finalizedBlockId: BlockId, blockId: BlockId, signature: ByteStr)
  case Valid(endorser: PublicKey, finalizedBlockId: BlockId, signature: ByteStr)
}

object BlockEndorsement {
  def sign(privateKey: PrivateKey, finalizedId: BlockId, id: BlockId, height: Height): ByteStr =
    crypto.sign(privateKey, mkMessage(finalizedId, id, height))

  def mkMessage(finalizedId: BlockId, id: BlockId, height: Height): Array[Byte] = finalizedId.arr ++ id.arr ++ Ints.toByteArray(height)

  def full(account: SeedKeyPair, finalizedId: BlockId, id: BlockId, height: Height): BlockEndorsement.Full =
    BlockEndorsement.Full(account.publicKey, finalizedId, id, height, sign(account.privateKey, finalizedId, id, height))
}
