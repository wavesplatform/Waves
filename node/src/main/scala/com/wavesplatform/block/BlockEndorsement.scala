package com.wavesplatform.block

import com.google.common.primitives.Ints
import com.wavesplatform.account.{PrivateKey, PublicKey, SeedKeyPair}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.Height

enum BlockEndorsement {
  case Full(endorser: PublicKey, blockId: BlockId, blockHeight: Height, signature: ByteStr)
  case Valid(endorser: PublicKey, signature: ByteStr)
  case Conflict(endorser: PublicKey, signature: ByteStr, referenceHash: ByteStr)
}

object BlockEndorsement {
  def sign(privateKey: PrivateKey, id: BlockId, height: Height): ByteStr =
    crypto.sign(privateKey, id.arr ++ Ints.toByteArray(height))

  def full(account: SeedKeyPair, id: BlockId, height: Height): BlockEndorsement.Full =
    BlockEndorsement.Full(
      endorser = account.publicKey,
      blockId = id,
      blockHeight = height,
      signature = sign(account.privateKey, id, height)
    )
}
