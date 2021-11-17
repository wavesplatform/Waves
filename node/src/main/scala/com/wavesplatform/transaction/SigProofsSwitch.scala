package com.wavesplatform.transaction

trait SigProofsSwitch extends ProvenTransaction { self: Transaction with VersionedTransaction =>
  def usesLegacySignature: Boolean =
    self.version == Transaction.V1
}
