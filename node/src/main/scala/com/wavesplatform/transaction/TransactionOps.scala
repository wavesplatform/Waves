package com.wavesplatform.transaction

import com.wavesplatform.transaction.serialization.TxSerializer
import com.wavesplatform.transaction.sign.TxSigner
import com.wavesplatform.transaction.validation.TxValidator

trait TransactionOps {
  type TransactionT <: Transaction

  def signer: TxSigner[TransactionT]
  def serializer: TxSerializer[TransactionT]
  def validator: TxValidator[TransactionT]
}
