package com.wavesplatform.protobuf.block

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
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
      v.endorsers.map(_.byteStr.toByteString),
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
