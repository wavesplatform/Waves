package scorex.consensus.qora

trait QoraLikeConsensusBlockData {
  val generatingBalance: Long
  val generatorSignature: Array[Byte]
}
