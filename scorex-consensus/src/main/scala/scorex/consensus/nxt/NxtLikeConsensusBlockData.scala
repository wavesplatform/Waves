package scorex.consensus.nxt

trait NxtLikeConsensusBlockData {
  val baseTarget: Long
  val generationSignature: Array[Byte]
}
