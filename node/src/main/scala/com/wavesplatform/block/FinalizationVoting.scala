package com.wavesplatform.block

import com.wavesplatform.crypto.bls.BlsSignature

case class FinalizationVoting(
    endorserIndexes: Seq[Int] = Seq.empty,
    aggregatedEndorsement: BlsSignature = BlsSignature.Empty,
    conflict: IndexedSeq[BlockEndorsement.Conflict] = IndexedSeq.empty
) {
  def withConsistent(endorserIndex: Int, signature: BlsSignature.NonEmpty): FinalizationVoting = copy(
    endorserIndexes = endorserIndexes :+ endorserIndex,
    aggregatedEndorsement = aggregatedEndorsement.append(signature)
  )

  def nonEmpty: Boolean = endorserIndexes.nonEmpty || conflict.nonEmpty

  override def toString: String =
    s"Voting(i={${endorserIndexes.mkString(",")}}, c={${conflict.mkString(", ")}}, s=$aggregatedEndorsement)"
}
