package com.wavesplatform.block

import com.wavesplatform.crypto.bls.BlsSignature

case class FinalizationVoting(
    endorserIndexes: Seq[Int],
    aggregatedEndorsement: BlsSignature,
    conflict: Seq[BlockEndorsement.Conflict]
) {
  override def toString: String = s"Voting(i={${endorserIndexes.mkString(",")}}, s=$aggregatedEndorsement" +
    s"${if (conflict.isEmpty) "" else s"c={${conflict.mkString(", ")}}"})"

  def +(other: FinalizationVoting): FinalizationVoting = ???
}
