package com.wavesplatform.protobuf.block

import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*
import com.wavesplatform.state.{GeneratorIndex, Height}

import scala.util.Try

object PBFinalizationVotings {
  def vanilla(pb: PBFinalizationVoting): Try[VanillaFinalizationVoting] = Try {
    val aggSig =
      if (pb.aggregatedEndorsementSignature.isEmpty) None
      else Option(BlsSignature(pb.aggregatedEndorsementSignature.toByteArray).explicitGet())

    VanillaFinalizationVoting(
      GeneratorIndex.seq(pb.endorserIndexes),
      Height(pb.finalizedBlockHeight),
      aggSig,
      pb.conflictEndorsements.zipWithIndex.map { case (x, i) =>
        BlsSignature(x.signature.toByteArray).map(PBEndorseBlocks.vanilla(x, _)) match {
          case Left(e)  => throw new IllegalArgumentException(s"Error during parsing conflict endorsement #$i: $e")
          case Right(r) => r
        }
      }.toVector
    )
  }

  def protobuf(v: VanillaFinalizationVoting): PBFinalizationVoting = PBFinalizationVoting.of(
    GeneratorIndex.toInts(v.valid),
    v.finalizedHeight.toInt,
    v.aggregatedEndorsement.fold(ByteString.EMPTY)(_.byteStr.toByteString),
    v.conflict.map(PBEndorseBlocks.protobuf)
  )
}
