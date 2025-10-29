package com.wavesplatform.block

import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.state.GeneratorIndex

case class FinalizationVoting(
    valid: Seq[GeneratorIndex] = Seq.empty,
    aggregatedEndorsement: BlsSignature = BlsSignature.Empty,
    conflict: IndexedSeq[BlockEndorsement.Conflict] = IndexedSeq.empty
) {
  def withValid(endorser: GeneratorIndex, signature: BlsSignature.NonEmpty): FinalizationVoting = copy(
    valid = valid :+ endorser,
    aggregatedEndorsement = aggregatedEndorsement.append(signature)
  )

  def nonEmpty: Boolean = valid.nonEmpty || conflict.nonEmpty

  override def toString: String =
    s"Voting(v=[${valid.mkString(",")}], c=[${conflict.mkString(", ")}], s=$aggregatedEndorsement)"
}
