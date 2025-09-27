package com.wavesplatform.block

import com.wavesplatform.crypto.bls.BlsSignature

case class FinalizationVoting(
    endorserIndexes: Seq[Int] = Seq.empty,
    aggregatedEndorsement: BlsSignature = BlsSignature.Empty,
    conflict: Seq[BlockEndorsement.Conflict] = Seq.empty
) {
  def withSignature(signature: BlsSignature.NonEmpty): FinalizationVoting = copy(aggregatedEndorsement = signature)

  def withValid(endorserIndex: Int, signature: BlsSignature.NonEmpty): FinalizationVoting = copy(
    endorserIndexes = endorserIndexes :+ endorserIndex,
    aggregatedEndorsement = aggregatedEndorsement.append(signature)
  )

  def withConflict(v: BlockEndorsement.Conflict): FinalizationVoting = copy(conflict = conflict :+ v)

  override def toString: String =
    s"Voting(i={${endorserIndexes.mkString(",")}}, s=$aggregatedEndorsement${if (conflict.isEmpty) "" else s"c={${conflict.mkString(", ")}}"})"
}
