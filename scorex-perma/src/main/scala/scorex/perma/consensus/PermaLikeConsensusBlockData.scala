package scorex.perma.consensus

import scorex.perma.actors.PartialProof

trait PermaLikeConsensusBlockData {
  val s:Array[Byte]
  val proofs: IndexedSeq[PartialProof]
}
