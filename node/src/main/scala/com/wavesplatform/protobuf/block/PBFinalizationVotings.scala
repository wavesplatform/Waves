package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*

import scala.util.Try

object PBFinalizationVotings {
  def vanilla(pb: PBFinalizationVoting): Try[VanillaFinalizationVoting] = Try {
    val sig = BlsSignature(pb.aggregatedEndorsementSignature.toByteArray).explicitGet() // TODO: better error
    VanillaFinalizationVoting(
      pb.endorserIndexes,
      sig,
      pb.conflictEndorsements.zipWithIndex.map { case (x, i) =>
        PBEndorseBlocks.vanilla(x, sig) match {
          case x: BlockEndorsement.Conflict => x
          case _                            => throw new IllegalArgumentException(s"EndorseBlock $i is incomplete")
        }
      }
    )
  }

  def protobuf(v: VanillaFinalizationVoting): PBFinalizationVoting =
    new PBFinalizationVoting(
      v.endorserIndexes,
      v.aggregatedEndorsement.byteStr.toByteString,
      v.conflict.map { x =>
        PBEndorseBlock(
          x.endorserIndex,
          x.finalizedId.toByteString,
          signature = x.signature.byteStr.toByteString
        )
      }
    )
}
