package com.wavesplatform.transaction

import com.wavesplatform.transaction.assets.exchange.Order
import shapeless._

package object smart {
  object InputPoly extends Poly1 {
    implicit def caseOrd = at[Order](o => RealTransactionWrapper.ord(o))
    implicit def caseTx  = at[Transaction](tx => RealTransactionWrapper(tx))
  }
}
