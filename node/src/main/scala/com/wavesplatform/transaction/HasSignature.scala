package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr

trait HasSignature { self: Proven & Versioned =>
  def usesLegacySignature: Boolean =
    self.version == Transaction.V1

  def signature: ByteStr = self.proofs.toSignature
}
