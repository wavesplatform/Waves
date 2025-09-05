package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*

import scala.util.Try

object PBFinalizationVotings {
  def vanilla(pb: PBFinalizationVoting): Try[VanillaFinalizationVoting] = Try {
    VanillaFinalizationVoting(
      pb.endorserIndexes,
      BlsSignature(pb.aggregatedEndorsementSignature.toByteArray),
      pb.conflictEndorsements.zipWithIndex.map { case (x, i) =>
        PBEndorseBlocks.vanilla(x) match {
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
          x.endorser.byteStr.toByteString,
          x.finalizedBlockId.toByteString,
          x.blockId.toByteString,
          signature = x.signature.byteStr.toByteString
        )
      }
    )
}
