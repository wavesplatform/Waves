package com.wavesplatform.network

import com.typesafe.scalalogging.LazyLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.network.EndorsementStorage.EndorsementFilter
import com.wavesplatform.state.Height
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

import scala.collection.mutable

// TODO: .switch: use in appender when changed height
trait EndorsementStorage {

  /** Add a vote, preserves the order of voting.
    * @return
    *   true, if it can be shared with neighbours
    */
  def tryAddVote(msg: EndorseBlock): Boolean

  def startNewVoting(filter: EndorsementFilter): Unit

  /** Returns a voting results at this time and resets all except an aggregated voting signature.
    */
  def tryCollectAndClear(endorsedBlockId: BlockId): Option[FinalizationVoting]
}

object EndorsementStorage {
  type EndorserIndex = Int
  case class EndorsementFilter(endorsedHeight: Height, endorsedId: BlockId, finalizedId: BlockId, expectedEndorsers: Map[BlsPublicKey, EndorserIndex])

  val Disabled: EndorsementStorage = new EndorsementStorage {
    override def tryAddVote(msg: EndorseBlock): Boolean                                   = false
    override def startNewVoting(filter: EndorsementFilter): Unit                          = {}
    override def tryCollectAndClear(endorsedBlockId: BlockId): Option[FinalizationVoting] = None
  }

  // Logs?
  class InMemory extends EndorsementStorage {
    private var currentFilter = Option.empty[EndorsementFilter]
    private val processed     = mutable.HashSet.empty[EndorseBlock]

    private var currentVoting = FinalizationVoting() // TODO: move to currentFilter?

    private val monitor            = new Object()
    private def synced[T](f: => T) = monitor.synchronized(f)

    // TODO: move?
    private def verify(msg: EndorseBlock): Option[BlsSignature.NonEmpty] = {
      val pk = BlsPublicKey(msg.endorserPublicKey)
      for {
        sig <- BlsSignature(msg.signature).toOption
        _   <- Option.when(pk.verify(BlockEndorsement.mkMessage(msg.finalizedBlockId, msg.blockId, msg.blockHeight), sig))(sig)
      } yield sig
    }

    override def tryAddVote(msg: EndorseBlock): Boolean = synced {
      currentFilter.exists { filter => // Empty means "no voting for now"
        msg.blockHeight == filter.endorsedHeight &&
        msg.blockId == filter.endorsedId && // Ignore endorsing other block because this could be a switch to a better branch
        !processed.contains(msg) && {
          val pk = BlsPublicKey(msg.endorserPublicKey)
          // We don't track a number of endorsers, because it guaranteed by CommitToGeneration transaction validation
          (verify(msg), filter.expectedEndorsers.get(pk)) match {
            case (Some(sig), Some(idx)) =>
              val isConflict = msg.finalizedBlockId == filter.finalizedId
              // TODO: wrong
              currentVoting = if (processed.isEmpty) { // First vote
                if (isConflict) FinalizationVoting(conflict = Seq(toConflict(msg, pk, sig)))
                else FinalizationVoting(aggregatedEndorsement = sig)
              } else if (isConflict) currentVoting.withValid(idx, sig)
              else currentVoting.withConflict(toConflict(msg, pk, sig))

              processed += msg
              true

            case _ => false // Either empty signature or unexpected endorser
          }
        }
      }
    }

    private def toConflict(msg: EndorseBlock, pk: BlsPublicKey, verifiedSig: BlsSignature.NonEmpty): BlockEndorsement.Conflict =
      BlockEndorsement.Conflict(pk, msg.finalizedBlockId, msg.blockId, verifiedSig)

    override def startNewVoting(filter: EndorsementFilter): Unit = synced {
      currentFilter = Some(filter)
      processed.clear()
      currentVoting = FinalizationVoting()
    }

    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = synced {
      currentFilter.filter(_.endorsedId == endorsedId).map { _ =>
        val r = currentVoting
        currentVoting = currentVoting.copy(endorserIndexes = Seq.empty, conflict = Seq.empty)
        r
      }
    }
  }
}

object EndorseBlockSynchronizer extends LazyLogging {
  def start(
      storage: EndorsementStorage,
      lastFilter: Observable[EndorsementFilter],
      receivingEndorsements: Observable[(Channel, EndorseBlock)],
      allChannels: DefaultChannelGroup,
      scheduler: Scheduler
  ): Cancelable = {
    // TODO: move outside
    lastFilter.foreach(storage.startNewVoting)(using scheduler)

    receivingEndorsements.foreach { case (ch, x) =>
      if (storage.tryAddVote(x)) allChannels.broadcast(x, Some(ch))
    }(using scheduler)
  }
}
