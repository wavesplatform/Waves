package com.wavesplatform.block

import com.wavesplatform.bls.{BlsPublicKey, BlsSignature}

case class FinalizationVoting(
    endorsers: Seq[BlsPublicKey],
    aggregatedEndorsement: BlsSignature,
    conflict: Seq[BlockEndorsement.Conflict]
) {
  override def toString: String = s"Voting(v=${endorsers.size}, s=${aggregatedEndorsement.base64Raw}" +
    s"${if (conflict.isEmpty) "" else s"c={${conflict.mkString(", ")}}"})"

  def +(other: FinalizationVoting): FinalizationVoting = ???
}
