package com.wavesplatform.transaction

import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.lang.v1.compiler.Terms.CaseObj
import shapeless._

package object smart {
  object InputPoly extends Poly1 {
    implicit def caseOrd = at[Order](o => RealTransactionWrapper.ord(o))
    implicit def caseTx  = at[Transaction](tx => RealTransactionWrapper(tx))
    implicit def caseObj = at[CaseObj](o => o)
  }
}
