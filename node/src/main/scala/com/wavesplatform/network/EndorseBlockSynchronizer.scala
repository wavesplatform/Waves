package com.wavesplatform.network

import cats.syntax.either.*
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}
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
  case class EndorsementFilter(finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId, expectedEndorsers: IndexedSeq[BlsPublicKey])

  val Disabled: EndorsementStorage = new EndorsementStorage {
    override def tryAddVote(msg: EndorseBlock): Boolean                                   = false
    override def startNewVoting(filter: EndorsementFilter): Unit                          = {}
    override def tryCollectAndClear(endorsedBlockId: BlockId): Option[FinalizationVoting] = None
  }

  // Logs?
  class InMemory extends EndorsementStorage with StrictLogging {
    private var currentFilter = Option.empty[EndorsementFilter]
    private val processed     = mutable.HashSet.empty[EndorseBlock]

    private var currentVoting = FinalizationVoting() // TODO: move to currentFilter?

    private val monitor            = new Object()
    private def synced[T](f: => T) = monitor.synchronized(f)

    // TODO: move?
    private def verifySig(msg: EndorseBlock, pk: BlsPublicKey): Option[BlsSignature.NonEmpty] =
      for {
        sig <- BlsSignature(msg.signature).toOption
        _   <- Option.when(pk.verify(BlockEndorsement.mkMessage(msg.finalizedId, msg.finalizedHeight, msg.endorsedId), sig))(sig)
      } yield sig

    override def tryAddVote(msg: EndorseBlock): Boolean = synced {
      for {
        filter <- currentFilter.toRight("Voting hasn't started")
        _      <- Either.raiseUnless(msg.finalizedHeight == filter.finalizedHeight)(s"Expected finalized height ${filter.finalizedHeight}")
        _      <- Either.raiseWhen(msg.endorserIndex >= filter.expectedEndorsers.size)(s"There are only ${filter.expectedEndorsers.size} endorsers")
        _      <- Either.raiseWhen(processed.contains(msg))("Already processed")
        endorserPk = filter.expectedEndorsers(msg.endorserIndex)
        sig <- verifySig(msg, endorserPk).toRight("Invalid signature")
        _   <- Either.raiseUnless(msg.endorsedId == filter.endorsedId)(s"Expected block ${filter.endorsedId}") // Could be a switch to a better branch
      } yield {
        // TODO: Tests
        val isConsistent = msg.finalizedId == filter.finalizedId
        currentVoting = if (isConsistent) {
          if (currentVoting.endorserIndexes.isEmpty) FinalizationVoting(aggregatedEndorsement = sig) // First vote
          else currentVoting.withValid(msg.endorserIndex, sig)
        } else currentVoting.withConflict(toConflict(msg, sig))

        processed += msg
        true
      }
    } match {
      case Left(err) => logger.trace(s"Unexpected $msg: $err"); false
      case Right(r)  => r
    }

    private def toConflict(msg: EndorseBlock, verifiedSig: BlsSignature.NonEmpty): BlockEndorsement.Conflict =
      BlockEndorsement.Conflict(msg.endorserIndex, msg.finalizedId, msg.endorsedId, verifiedSig)

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
