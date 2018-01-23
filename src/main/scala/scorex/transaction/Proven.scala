package scorex.transaction

import com.wavesplatform.Proofs

trait Proven extends Authorized {
  def proofs: Proofs
}
