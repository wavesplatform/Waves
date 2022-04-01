package com.wavesplatform.transaction.validation.impl

import cats.syntax.validated._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.validation.{TxValidator, _}

object SetScriptTxValidator extends TxValidator[SetScriptTransaction] {
  override def validate(tx: SetScriptTransaction): ValidatedV[SetScriptTransaction] = tx.validNel
}
