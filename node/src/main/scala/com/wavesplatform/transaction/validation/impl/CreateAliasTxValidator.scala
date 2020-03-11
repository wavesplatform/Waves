package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object CreateAliasTxValidator extends TxValidator[CreateAliasTransaction] {
  override def validate(tx: CreateAliasTransaction): ValidatedV[CreateAliasTransaction] = {
    import tx._
    V.seq(tx)(
      V.fee(fee),
      V.addressChainId(alias, chainId)
    )
  }
}
