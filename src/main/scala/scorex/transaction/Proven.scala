package scorex.transaction

trait Proven extends Authorized {
  def proofs: Proofs
}
