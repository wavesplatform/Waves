package com.wavesplatform.block

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey, BlsSignature}
import com.wavesplatform.state.{GeneratorIndex, Height}

case class BlockEndorsement(
    endorserIndex: GeneratorIndex,
    finalizedId: BlockId,
    finalizedHeight: Height,
    endorsedId: BlockId,
    signature: BlsSignature
) {
  def signatureValid(endorserPublicKey: BlsPublicKey): Either[String, Unit] =
    signature.verifyBasic(BlockEndorsement.mkMessage(finalizedId, finalizedHeight, endorsedId), endorserPublicKey)
}

object BlockEndorsement {
  def signed(
      endorserAccount: BlsKeyPair,
      endorserIndex: GeneratorIndex,
      finalizedId: BlockId,
      finalizedHeight: Height,
      endorsedId: BlockId
  ): BlockEndorsement =
    BlockEndorsement(endorserIndex, finalizedId, finalizedHeight, endorsedId, sign(endorserAccount, finalizedId, finalizedHeight, endorsedId))

  def sign(kp: BlsKeyPair, finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): BlsSignature =
    kp.sign(mkMessage(finalizedId, finalizedHeight, endorsedId))

  def mkMessage(finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): Array[Byte] =
    finalizedId.arr ++ finalizedHeight.toByteArray ++ endorsedId.arr
}
