package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.CommitToGenerationTransaction
import com.wavesplatform.transaction.validation.*

object CommitToGenerationTxValidator extends TxValidator[CommitToGenerationTransaction] {
  override def validate(tx: CommitToGenerationTransaction): ValidatedV[CommitToGenerationTransaction] = {
    V.seq(tx)(
      V.cond(true, ???) // TODO: Check PK
    )
  }
}
