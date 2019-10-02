package com.wavesplatform.transaction

import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.transaction.assets.exchange.Order
import shapeless._

package object smart {
  object InputPoly extends Poly1 {
    implicit def caseOrd        = at[Order](o => RealTransactionWrapper.ord(o))
    implicit def scriptTransfer = at[ScriptTransfer](o => o)
    implicit def caseTx(implicit multiPaymentAllowed: Boolean, v: StdLibVersion) =
      at[Transaction](tx => RealTransactionWrapper(tx, multiPaymentAllowed, v))
  }

  object PaymentsInputPoly extends Poly1 {
    implicit def caseOrd        = at[Order](o => RealTransactionWrapper.ord(o))
    implicit def scriptTransfer = at[ScriptTransfer](o => o)
    implicit def caseTx =        at[Transaction](tx => RealTransactionWrapper(tx, multiPaymentAllowed, v))

  }
}
