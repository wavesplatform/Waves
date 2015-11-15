package scorex.perma.consensus

import scorex.perma.actors.Ticket

//case class BlockHeaderLike(difficulty: BigInt, puz: Array[Byte], ticket: Ticket)

trait PermaLikeConsensusBlockData {
  val difficulty: BigInt
  val puz: Array[Byte]
  val ticket: Ticket
}
