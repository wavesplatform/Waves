package com.wavesplatform.state

import cats.syntax.either.*
import cats.syntax.option.*
import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.state.EndorsementFilter.SimulationResult
import com.wavesplatform.state.EndorsementStorage.InMemory.FinalizationResult
import com.wavesplatform.state.Height

import scala.collection.{immutable, mutable}

// TODO: .switch: use in appender when changed height
trait EndorsementStorage {

  /** @return true, if it can be shared with neighbours
    */
  def tryAdd(msg: EndorseBlock): Either[String, Boolean]

  /** @return true if it is a new voting */
  def startVoting(filter: EndorsementFilter): Boolean

  /** @return
    *   A voting result snapshot with minimal required votes if we got conflicting endorsements, reached finalization or lost
    *   None if there are no updates since last attempt.
    */
  def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting]
}

object EndorsementStorage {
  object Disabled extends EndorsementStorage {
    override def tryAdd(msg: EndorseBlock): Either[String, Boolean]                  = true.asRight
    override def startVoting(filter: EndorsementFilter): Boolean                     = false
    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = None
  }

  class InMemory(blockAtHeight: (BlockId, Height) => Boolean) extends EndorsementStorage, StrictLogging {
    private var currentFilter = none[EndorsementFilter] // TODO: remove option?

    private val sharedWithNeighbors     = mutable.HashSet.empty[EndorseBlock]
    private val processedValidEndorsers = mutable.HashSet.empty[GeneratorIndex]

    private var valid    = immutable.IntMap.empty[BlsSignature.NonEmpty]
    private var conflict = immutable.IntMap.empty[BlockEndorsement]

    private var latestResult = FinalizationResult.empty
    private var hasChanges   = false

    private val monitor            = new Object()
    private def synced[T](f: => T) = monitor.synchronized(f)

    override def tryAdd(msg: EndorseBlock): Either[String, Boolean] = synced {
      for {
        filter        <- currentFilter.toRight("Voting hasn't started")
        _             <- Either.raiseWhen(msg.finalizedHeight > filter.finalizedHeight)(s"Expected finalized height <= ${filter.finalizedHeight}")
        _             <- Either.raiseWhen(msg.endorserIndex >= filter.endorsers.size)(s"There are only ${filter.endorsers.size} endorsers")
        endorserIndex <- GeneratorIndex.checked(msg.endorserIndex).toRight(s"Invalid endorser index: ${msg.endorserIndex}")
        (_, endorserPk, _) = filter.endorsers(msg.endorserIndex)
        sig <- verifySig(msg, endorserPk).toRight("Invalid signature")
      } yield
        if (sharedWithNeighbors.contains(msg) || conflict.isDefinedAt(msg.endorserIndex) || filter.conflict.contains(endorserIndex)) false
        else {
          val isValid = msg.finalizedHeight == filter.finalizedHeight && msg.finalizedId == filter.finalizedId
          val isConflict = !isValid && {
            msg.finalizedHeight == filter.finalizedHeight && msg.finalizedId != filter.finalizedId ||
            msg.finalizedHeight < filter.finalizedHeight && !blockAtHeight(msg.finalizedId, msg.finalizedHeight)
          }

          val share = if (isConflict) {
            conflict = conflict.updated(
              msg.endorserIndex,
              BlockEndorsement(GeneratorIndex(msg.endorserIndex), msg.finalizedId, msg.finalizedHeight, msg.endorsedId, sig)
            )
            valid = valid.removed(msg.endorserIndex)

            true
          } else if (isValid && msg.endorsedId == filter.endorsedId && !processedValidEndorsers.contains(endorserIndex)) {
            valid = valid.updated(msg.endorserIndex, sig)
            processedValidEndorsers.add(endorserIndex)

            true
          } else false

          if (share) {
            hasChanges = true
            sharedWithNeighbors += msg
          }

          share && filter.miner.isEmpty
        }
    }

    override def startVoting(filter: EndorsementFilter): Boolean = synced {
      val isNewVoting = !currentFilter.exists(_.sameVoting(filter))
      if (isNewVoting) {
        sharedWithNeighbors.clear()
        processedValidEndorsers.clear()

        valid = valid.empty
        conflict = conflict.empty

        latestResult = FinalizationResult.empty
        hasChanges = false

        currentFilter = if (filter.endorsers.isEmpty) {
          logger.info("No committed generators, don't collect endorsements")
          none
        } else {
          logger.info(s"Started voting with $filter")
          filter.some
        }
      } else logger.trace(s"Same voting: current=$currentFilter vs new=$filter")
      isNewVoting
    }

    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = synced {
      for {
        currentFilter <- currentFilter
        if currentFilter.endorsedId == endorsedId && hasChanges
        _ = {
          hasChanges = false
        }

        moreConflict   = conflict.size > latestResult.voting.conflict.size
        moreValid      = valid.size > latestResult.voting.valid.size
        couldFinalized = !latestResult.reachedFinalization && moreValid
        if moreConflict || couldFinalized

        origResult = latestResult
        _ = {
          val simulation = currentFilter.simulate(valid.keys, conflict.keySet)
          latestResult = createVoting(currentFilter, simulation)
        }
        changedFinalizationStatus = latestResult.reachedFinalization != origResult.reachedFinalization
        if moreConflict || changedFinalizationStatus
      } yield latestResult.voting
    }

    private def createVoting(currentFilter: EndorsementFilter, simulationResult: SimulationResult): FinalizationResult = {
      val votingWithoutValid = FinalizationVoting(
        valid = Seq.empty,
        finalizedHeight = currentFilter.finalizedHeight,
        aggregatedEndorsement = BlsSignature.Empty,
        conflict = conflict.values.toIndexedSeq
      )

      val voting =
        if (simulationResult.reachedFinalization)
          simulationResult.chosenValid.foldLeft(votingWithoutValid) { case (r, idx) => r.withValid(idx, valid(idx.toInt)) }
        else votingWithoutValid
      FinalizationResult(simulationResult.reachedFinalization, voting)
    }

    private def verifySig(msg: EndorseBlock, pk: BlsPublicKey): Option[BlsSignature.NonEmpty] =
      for {
        sig <- BlsSignature(msg.signature).toOption
        _   <- Option.when(pk.verify(BlockEndorsement.mkMessage(msg.finalizedId, msg.finalizedHeight, msg.endorsedId), sig))(sig)
      } yield sig
  }

  object InMemory {
    private case class FinalizationResult(reachedFinalization: Boolean, voting: FinalizationVoting)
    private object FinalizationResult {
      val empty = FinalizationResult(
        reachedFinalization = false,
        FinalizationVoting(
          valid = Seq.empty,
          finalizedHeight = GenesisBlockHeight,
          aggregatedEndorsement = BlsSignature.Empty,
          conflict = IndexedSeq.empty
        )
      )
    }
  }
}
