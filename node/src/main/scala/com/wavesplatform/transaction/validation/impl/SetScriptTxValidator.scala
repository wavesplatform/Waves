package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.validation.{TxValidator, *}

object SetScriptTxValidator extends TxValidator[SetScriptTransaction] {
  override def validate(tx: SetScriptTransaction): ValidatedV[SetScriptTransaction] = {
    val isUnionInCallableAllowed = tx.script match {
      case Some(sc: ContractScriptImpl) => sc.isUnionInCallableAllowed
      case _ => Right(true)
    }

    V.seq(tx)(
      V.cond(tx.script.forall(!_.isFreeCall), GenericError("Script type for Set Script Transaction should not be CALL")),
      V.cond(
        isUnionInCallableAllowed.contains(true),
        isUnionInCallableAllowed match {
          case Right(_) => GenericError("Union type is not allowed in callable function arguments of script")
          case Left(err) => GenericError(err)
        }
      )
    )
  }
}
