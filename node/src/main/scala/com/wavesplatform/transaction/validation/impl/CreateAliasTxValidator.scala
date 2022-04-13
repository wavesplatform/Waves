package com.wavesplatform.transaction.validation.impl

import cats.data.Validated
import com.wavesplatform.account.Alias
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object CreateAliasTxValidator extends TxValidator[CreateAliasTransaction] {
  override def validate(tx: CreateAliasTransaction): ValidatedV[CreateAliasTransaction] = {
    import tx._
    V.seq(tx)(
      Validated.fromEither(Alias.createWithChainId(aliasName, chainId)).toValidatedNel.map((_: Alias) => tx)
    )
  }
}
