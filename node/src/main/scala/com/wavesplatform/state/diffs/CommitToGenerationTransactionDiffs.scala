package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.{Asset, CommitToGenerationTransaction}

object CommitToGenerationTransactionDiffs {
  def apply(blockchain: Blockchain)(tx: CommitToGenerationTransaction): Either[ValidationError, StateSnapshot] = {
    for {
      // TODO: Check BLS signature
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = Map(tx.sender.toAddress -> Portfolio.build(Asset.Waves -> -tx.fee.value)),
        nextCommittedGenerators = Map(tx.sender -> tx.endorsementPublicKey)
      )
    } yield snapshot
  }
}
