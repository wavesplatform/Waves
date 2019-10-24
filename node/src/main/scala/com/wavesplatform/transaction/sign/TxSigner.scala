package com.wavesplatform.transaction.sign

import com.wavesplatform.account.PrivateKey
import com.wavesplatform.transaction.Transaction

trait TxSigner[T <: Transaction] {
  def sign(tx: T, privateKey: PrivateKey): T
}
