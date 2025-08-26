package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.protobuf.*

import scala.util.Try

object PBFinalizationVotings {
  def vanilla(pb: PBFinalizationVoting): Try[VanillaFinalizationVoting] = Try {
    VanillaFinalizationVoting(
      pb.endorserPublicKeys.map(bytes => BlsPublicKey(bytes.toByteArray)),
      BlsSignature(pb.aggregatedEndorsementSignature.toByteArray),
      pb.invalidEndorsements.zipWithIndex.map { case (x, i) =>
        PBEndorseBlocks.vanilla(x) match {
          case x: BlockEndorsement.Conflict => x
          case _                            => throw new IllegalArgumentException(s"EndorseBlock $i is incomplete")
        }
      }
    )
  }

  def protobuf(v: VanillaFinalizationVoting): PBFinalizationVoting =
    new PBFinalizationVoting(
      v.endorsers.map(_.toByteString),
      v.aggregatedEndorsement.toByteString,
      v.invalid.map { x =>
        PBEndorseBlock(
          x.endorser.toByteString,
          x.finalizedBlockId.toByteString,
          x.blockId.toByteString,
          signature = x.signature.toByteString
        )
      }
    )
}
