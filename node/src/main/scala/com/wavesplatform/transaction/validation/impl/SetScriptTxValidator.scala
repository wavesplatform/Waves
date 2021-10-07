package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.validation.{TxValidator, _}

object SetScriptTxValidator extends TxValidator[SetScriptTransaction] {
  override def validate(tx: SetScriptTransaction): ValidatedV[SetScriptTransaction] =
    V.seq(tx)(
      V.fee(tx.fee).map(_ => tx),
      V.cond(tx.script.forall(!_.isFreeCall), GenericError("Script type for Set Script Transaction should not be CALL"))
    )
}
