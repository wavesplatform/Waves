package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object SetAssetScriptTxValidator extends TxValidator[SetAssetScriptTransaction] {
  override def validate(tx: SetAssetScriptTransaction): ValidatedV[SetAssetScriptTransaction] = {
    import tx.*
    V.seq(tx)(
      V.cond(
        script.forall(_.isInstanceOf[ExprScript]),
        GenericError(s"Asset can only be assigned with Expression script, not Contract")
      ),
      V.cond(
        script.isDefined,
        GenericError("Cannot set empty script")
      )
    )
  }
}
