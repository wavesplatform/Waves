package scorex.perma.consensus

import scorex.perma.actors.Ticket

trait PermaLikeConsensusBlockData {
  val difficulty: BigInt
  val puz: Array[Byte]
  val ticket: Ticket
}
