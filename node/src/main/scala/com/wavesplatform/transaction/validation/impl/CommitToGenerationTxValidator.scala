package com.wavesplatform.transaction.validation.impl

import cats.data.Validated.Valid
import com.wavesplatform.transaction.CommitToGenerationTransaction
import com.wavesplatform.transaction.validation.*

object CommitToGenerationTxValidator extends TxValidator[CommitToGenerationTransaction] {
  override def validate(tx: CommitToGenerationTransaction): ValidatedV[CommitToGenerationTransaction] =
    Valid(tx) // Nothing to validate
}
