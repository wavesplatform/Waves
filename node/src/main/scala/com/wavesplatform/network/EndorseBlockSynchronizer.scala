package com.wavesplatform.network

import cats.syntax.either.*
import cats.syntax.option.*
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

  def startVoting(filter: EndorsementFilter): Unit

  /** Returns a voting results at this time and resets all except an aggregated voting signature.
    */
  def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting]
}

object EndorsementStorage {
  case class EndorsementFilter(finalizedId: BlockId, finalizedHeight: Height, endorsedId: BlockId, expectedEndorsers: IndexedSeq[BlsPublicKey]) {
    override def toString: String =
      s"EndorsementFilter(fid=$finalizedId, fh=$finalizedHeight, eid=$endorsedId, e={${expectedEndorsers.mkString(", ")}})"

    def sameVoting(other: EndorsementFilter): Boolean =
      finalizedId == other.finalizedId && finalizedHeight == other.finalizedHeight && endorsedId == other.endorsedId
  }

  val Disabled: EndorsementStorage = new EndorsementStorage {
    override def tryAddVote(msg: EndorseBlock): Boolean                              = false
    override def startVoting(filter: EndorsementFilter): Unit                        = {}
    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = None
  }

  class InMemory extends EndorsementStorage with StrictLogging {
    private var currentFilter = Option.empty[EndorsementFilter]
    private var currentVoting = Option.empty[FinalizationVoting]
    private val processed     = mutable.HashSet.empty[EndorseBlock]

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
        filter     <- currentFilter.toRight("Voting hasn't started")
        origVoting <- currentVoting.toRight("Voting hasn't started")
        _          <- Either.raiseUnless(msg.finalizedHeight == filter.finalizedHeight)(s"Expected finalized height ${filter.finalizedHeight}")
        _ <- Either.raiseWhen(msg.endorserIndex >= filter.expectedEndorsers.size)(s"There are only ${filter.expectedEndorsers.size} endorsers")
        _ <- Either.raiseWhen(processed.contains(msg))("")
        endorserPk = filter.expectedEndorsers(msg.endorserIndex)
        sig <- verifySig(msg, endorserPk).toRight("Invalid signature")
        _   <- Either.raiseUnless(msg.endorsedId == filter.endorsedId)(s"Expected block ${filter.endorsedId}") // Could be a switch to a better branch
      } yield {
        // TODO: Tests
        val isConsistent = msg.finalizedId == filter.finalizedId
        val updatedVoting =
          if (isConsistent) origVoting.withValid(msg.endorserIndex, sig)
          else origVoting.withConflict(toConflict(msg, sig))

        currentVoting = updatedVoting.some
        processed += msg
      }
    } match {
      case Left("")  => false // Ignore without logs
      case Left(err) => logger.trace(s"Unexpected $msg: $err"); false
      case Right(_)  => true
    }

    private def toConflict(msg: EndorseBlock, verifiedSig: BlsSignature.NonEmpty): BlockEndorsement.Conflict =
      BlockEndorsement.Conflict(msg.endorserIndex, msg.finalizedId, verifiedSig)

    override def startVoting(filter: EndorsementFilter): Unit = synced {
      val isNewVoting = !currentFilter.exists(_.sameVoting(filter))
      if (isNewVoting) {
        currentVoting = None
        processed.clear()

        currentFilter = if (filter.expectedEndorsers.isEmpty) {
          logger.info("No committed generators, don't collect endorsements")
          None
        } else {
          logger.info(s"Started voting with $filter")
          Some(filter)
        }
      }
    }

    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = synced {
      for {
        currentFilter <- currentFilter
        if currentFilter.endorsedId == endorsedId
        origVoting <- currentVoting
      } yield {
        currentVoting = Some(origVoting.copy(endorserIndexes = Seq.empty, conflict = Seq.empty))
        origVoting
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
    lastFilter.foreach(storage.startVoting)(using scheduler)

    receivingEndorsements.foreach { case (ch, x) =>
      if (storage.tryAddVote(x)) allChannels.broadcast(x, Some(ch))
    }(using scheduler)
  }
}
