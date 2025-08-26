package com.wavesplatform.block

import com.wavesplatform.bls.{BlsPublicKey, BlsSignature}

case class FinalizationVoting(
    endorsers: Seq[BlsPublicKey],
    aggregatedEndorsement: BlsSignature,
    invalid: Seq[BlockEndorsement.Conflict]
) {
  override def toString: String = s"Voting(v=${endorsers.size}, s=${aggregatedEndorsement.base64Raw}" +
    s"${if (invalid.isEmpty) "" else s"i={${invalid.mkString(", ")}}"})"
}
