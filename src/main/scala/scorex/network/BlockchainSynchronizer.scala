package scorex.network

import scorex.block.Block._
import scorex.crypto.encode.Base58.encode
import scorex.transaction.History._


object BlockchainSynchronizer {

  sealed trait Status {
    val name: String
  }

  case object GettingExtension extends Status {
    override val name = "getting extension"
  }

  case object GettingBlocks extends Status {
    override val name = "getting blocks"
  }

  case object Idle extends Status {
    override val name = "idle"
  }

  case object GetSyncStatus

  case class GetExtension(peerScores: Map[ConnectedPeer, BlockchainScore])

  case class InnerId(blockId: BlockId) {
    override def equals(obj: Any): Boolean = obj match {
      case InnerId(that) => blockId.sameElements(that)
      case _ => false
    }

    override def hashCode(): Int = scala.util.hashing.MurmurHash3.seqHash(blockId)

    override def toString: String = encode(blockId)
  }
}
