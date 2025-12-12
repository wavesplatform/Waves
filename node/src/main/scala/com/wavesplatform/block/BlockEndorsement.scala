package com.wavesplatform.block

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey, BlsSignature, BlsUtils}
import com.wavesplatform.state.{GeneratorIndex, Height}

case class BlockEndorsement(
    endorserIndex: GeneratorIndex,
    finalizedId: BlockId,
    finalizedHeight: Height,
    endorsedId: BlockId,
    signature: BlsSignature.NonEmpty
) {
  def signatureValid(endorserPublicKey: BlsPublicKey): Boolean =
    BlsUtils.verifyBasic(signature.byteStr.arr, BlockEndorsement.mkMessage(finalizedId, finalizedHeight, endorsedId), endorserPublicKey.arr)
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

  def sign(kp: BlsKeyPair, finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): BlsSignature.NonEmpty =
    kp.sign(mkMessage(finalizedId, finalizedHeight, endorsedId))

  def mkMessage(finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId): Array[Byte] =
    finalizedId.arr ++ finalizedHeight.toByteArray ++ endorsedId.arr
}
