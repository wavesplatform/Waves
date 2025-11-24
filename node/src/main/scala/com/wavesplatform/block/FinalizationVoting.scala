package com.wavesplatform.block

import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.state.{GeneratorIndex, Height}

case class FinalizationVoting(
    valid: Seq[GeneratorIndex] = Seq.empty,
    finalizedHeight: Height = Height(0),
    aggregatedEndorsement: BlsSignature = BlsSignature.Empty,
    conflict: IndexedSeq[BlockEndorsement] = IndexedSeq.empty
) {
  def withValid(endorser: GeneratorIndex, signature: BlsSignature.NonEmpty): FinalizationVoting = copy(
    valid = valid :+ endorser,
    aggregatedEndorsement = aggregatedEndorsement.append(signature)
  )

  def nonEmpty: Boolean = valid.nonEmpty || conflict.nonEmpty

  override def toString: String =
    s"Voting(v=[${valid.mkString(",")}], h=$finalizedHeight, c=[${conflict.mkString(", ")}], s=$aggregatedEndorsement)"
}
