package com.wavesplatform.transaction

import com.wavesplatform.account.PrivateKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto

trait ProvenTransaction extends Proven { this: Transaction =>
  type T <: Transaction
  def addProof(proof: ByteStr): T
}

object ProvenTransaction {
  extension (p: ProvenTransaction) {
    def signWith(privateKey: PrivateKey): p.T = p.addProof(crypto.sign(privateKey, p.bodyBytes()))
  }
}
