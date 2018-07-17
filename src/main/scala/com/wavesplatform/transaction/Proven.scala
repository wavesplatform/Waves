package com.wavesplatform.transaction

trait Proven extends Authorized {
  def proofs: Proofs
}
