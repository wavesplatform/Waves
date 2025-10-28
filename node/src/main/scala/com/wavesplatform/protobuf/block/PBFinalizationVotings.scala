package com.wavesplatform.protobuf.block

import cats.syntax.either.*
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.protobuf.*
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.Try

object PBFinalizationVotings {
  def vanilla(pb: PBFinalizationVoting): Try[VanillaFinalizationVoting] = Try {
    val aggSig =
      if (pb.aggregatedEndorsementSignature.isEmpty) BlsSignature.Empty
      else BlsSignature(pb.aggregatedEndorsementSignature.toByteArray).explicitGet()

    VanillaFinalizationVoting(
      pb.endorserIndexes,
      aggSig,
      pb.conflictEndorsements.zipWithIndex.map { case (x, i) =>
        val r = for {
          sig <- BlsSignature(x.signature.toByteArray)
          r   <- PBEndorseBlocks.vanillaConflict(x, sig).leftMap(GenericError(_))
        } yield r

        r match {
          case Left(e)  => throw new IllegalArgumentException(s"Error during parsing conflict endorsement #$i: $e")
          case Right(r) => r
        }
      }.toVector
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
