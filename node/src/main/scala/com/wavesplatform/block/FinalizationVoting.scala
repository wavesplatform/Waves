package com.wavesplatform.block

import com.wavesplatform.crypto.bls.BlsSignature
import com.wavesplatform.state.{GeneratorIndex, Height}
import com.wavesplatform.transaction.TxValidationError.GenericError

/** @param aggregatedEndorsement Empty if there is no valid endorsement (except miner's one)
  */
case class FinalizationVoting(
    valid: Seq[GeneratorIndex],
    finalizedHeight: Height,
    aggregatedEndorsement: Option[BlsSignature],
    conflict: Seq[BlockEndorsement]
) {
  def withValid(endorserIdxs: Iterable[GeneratorIndex], endorserSigs: Iterable[BlsSignature]): Either[GenericError, FinalizationVoting] =
    BlsSignature.agg(Iterable.concat(aggregatedEndorsement, endorserSigs)).map { agg =>
      copy(
        valid = valid ++ endorserIdxs,
        aggregatedEndorsement = Some(agg)
      )
    }

  def nonEmpty: Boolean = valid.nonEmpty || conflict.nonEmpty

  override def toString: String =
    s"Voting(v=[${valid.mkString(",")}], h=$finalizedHeight, c=[${conflict.mkString(", ")}], s=$aggregatedEndorsement)"
}

object FinalizationVoting {
  def combine(old: Option[FinalizationVoting], recent: Option[FinalizationVoting]): Option[FinalizationVoting] =
    (old, recent) match {
      case (r, None)                 => r
      case (None, r)                 => r
      case (Some(old), Some(recent)) => Some(FinalizationVoting.combine(old, recent))
    }

  def combine(old: FinalizationVoting, recent: FinalizationVoting): FinalizationVoting =
    recent.copy(conflict = old.conflict ++ recent.conflict)

  def isFinalized(endorsedBalance: BigInt, totalBalance: BigInt): Boolean = {
    // Same as: endorsedBalance >= totalBalance * 2/3
    // But solves a fraction issue:
    //  endorsed=7, total=11, required=7.(3), 7 < 7.(3) - not finalized with BigDecimal, finalized with BigInt (drops fraction part)
    //  endorsed * 3=21, total * 2=22, 21 < 22 - not finalized
    endorsedBalance * 3 >= totalBalance * 2
  }
}
