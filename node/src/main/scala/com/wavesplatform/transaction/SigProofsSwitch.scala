package com.wavesplatform.transaction

trait SigProofsSwitch extends ProvenTransaction { self: Transaction & Versioned =>
  def usesLegacySignature: Boolean =
    self.version == Transaction.V1
}
