package com.wavesplatform.block

import cats.kernel.Monoid
import com.wavesplatform.crypto.bls.BlsSignature

case class FinalizationVoting(
    endorserIndexes: Seq[Int] = Seq.empty,
    aggregatedEndorsement: BlsSignature = BlsSignature.Empty,
    conflict: Seq[BlockEndorsement.Conflict] = Seq.empty
) {
  def withValid(endorserIndex: Int, signature: BlsSignature.NonEmpty): FinalizationVoting = copy(
    endorserIndexes = endorserIndexes :+ endorserIndex,
    aggregatedEndorsement = aggregatedEndorsement.append(signature)
  )

  def withConflict(v: BlockEndorsement.Conflict): FinalizationVoting = copy(conflict = conflict :+ v)

  def hasUpdates: Boolean = endorserIndexes.nonEmpty || conflict.nonEmpty

  override def toString: String =
    s"Voting(i={${endorserIndexes.mkString(",")}}, c={${conflict.mkString(", ")}}, s=$aggregatedEndorsement)"
}

object FinalizationVoting {
  given Monoid[FinalizationVoting] = new Monoid[FinalizationVoting] {
    override val empty: FinalizationVoting = FinalizationVoting()
    override def combine(x: FinalizationVoting, y: FinalizationVoting): FinalizationVoting = FinalizationVoting(
      endorserIndexes = x.endorserIndexes ++ y.endorserIndexes,
      aggregatedEndorsement = y.aggregatedEndorsement,
      conflict = x.conflict ++ y.conflict
    )
  }
}
