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

import scala.collection.{immutable, mutable}

// TODO: .switch: use in appender when changed height
trait EndorsementStorage {

  /** @return true, if it can be shared with neighbors
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
    private var currentFilter = none[EndorsementFilter]

    private val sharedWithNeighbors     = mutable.HashSet.empty[EndorseBlock]
    private val processedValidEndorsers = mutable.HashSet.empty[GeneratorIndex]

    private var valid    = immutable.IntMap.empty[BlsSignature]
    private var conflict = immutable.IntMap.empty[BlockEndorsement]

    private var latestResult = FinalizationResult.empty
    private var hasChanges   = false

    private val monitor            = new Object()
    private def synced[T](f: => T) = monitor.synchronized(f)

    override def tryAdd(msg: EndorseBlock): Either[String, Boolean] = synced {
      for {
        filter <- currentFilter.toRight("Voting hasn't started")
        _ <- Either.raiseWhen(msg.finalizedHeight < GenesisBlockHeight || msg.finalizedHeight > filter.finalizedHeight) {
          s"Expected finalized height >= $GenesisBlockHeight and <= ${filter.finalizedHeight}"
        }
        _ <- Either.raiseWhen(msg.endorserIndex >= filter.normalizedGeneratorSet.size)(
          s"There are only ${filter.normalizedGeneratorSet.size} endorsers"
        )
        endorserIndex <- GeneratorIndex.checked(msg.endorserIndex).toRight(s"Invalid endorser index: ${msg.endorserIndex}")
        _             <- Either.raiseWhen(msg.endorserIndex == filter.miner.toInt)("Miner can't sent endorsements")
        (endorserAddr, endorserPk, balance) = filter.normalizedGeneratorSet(msg.endorserIndex)
        _   <- Either.raiseWhen(balance == 0)(s"Endorser #$endorserIndex $endorserAddr has no enough balance")
        sig <- verifySig(msg, endorserPk)
      } yield
        if (sharedWithNeighbors.contains(msg) || conflict.isDefinedAt(msg.endorserIndex) || filter.conflict.contains(endorserIndex)) false
        else {
          val isValid = msg.finalizedHeight == filter.finalizedHeight && msg.finalizedId == filter.finalizedId
          val isConflict = !isValid && {
            msg.finalizedHeight == filter.finalizedHeight && msg.finalizedId != filter.finalizedId ||
            msg.finalizedHeight < filter.finalizedHeight && !blockAtHeight(msg.finalizedId, msg.finalizedHeight)
          }

          val isNew = if (isConflict) {
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

          val share = isNew && !filter.isMiner
          if (isNew) {
            val kindStr = if (isConflict) "conflict" else "valid"
            logger.info(s"New $kindStr endorsement from #$endorserIndex $endorserAddr will${if (share) "" else " not"} be shared")

            hasChanges = true
            sharedWithNeighbors += msg
          } else logger.trace(s"Neither valid, nor conflict endorsement from #$endorserIndex")

          share
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

        currentFilter = if (filter.normalizedGeneratorSet.isEmpty) {
          logger.info("Generator set is empty, don't collect endorsements")
          none
        } else {
          logger.info(s"Started voting with $filter")
          filter.some
        }
      } else logger.trace(s"Same voting: current=$currentFilter vs new=$filter")
      isNewVoting
    }

    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = synced {
      val r = for {
        currentFilter <- currentFilter.toRight("Voting not started")
        _             <- Either.raiseUnless(currentFilter.endorsedId == endorsedId && hasChanges)("No changes")
        _ = {
          hasChanges = false
        }

        moreConflict   = conflict.size > latestResult.voting.conflict.size
        moreValid      = valid.size > latestResult.voting.valid.size
        couldFinalized = !latestResult.reachedFinalization && moreValid
        _ <- Either.raiseUnless(moreConflict || couldFinalized) {
          s"Could not be changed: finalized=${latestResult.reachedFinalization}, more valid=$moreValid, more conflict=$moreConflict"
        }

        origResult = latestResult
        simulation = currentFilter.simulate(valid.keys, conflict.keySet)
        newResult <- createVoting(currentFilter, simulation)
        _ = latestResult = newResult

        changedFinalizationStatus = newResult.reachedFinalization != origResult.reachedFinalization
        _ <- Either.raiseUnless(moreConflict || changedFinalizationStatus) {
          s"Status not changed, endorsed=${simulation.endorsedBalance}, total=${simulation.totalBalance}, chosen valid=[${simulation.chosenValid.sorted.mkString(", ")}], valid=[${valid.keysIterator.mkString(", ")}]"
        }
      } yield newResult.voting

      r.left.foreach { err =>
        if (currentFilter.nonEmpty) logger.debug(s"Not found new significant endorsements for $endorsedId: $err")
      }
      r.toOption
    }

    private def createVoting(currentFilter: EndorsementFilter, simulationResult: SimulationResult): Either[String, FinalizationResult] = {
      val votingWithoutValid = FinalizationVoting(
        valid = Seq.empty,
        finalizedHeight = currentFilter.finalizedHeight,
        aggregatedEndorsement = None,
        conflict = (conflict -- latestResult.voting.conflict.map(_.endorserIndex.toInt)).values.toIndexedSeq
      )

      val voting =
        if (simulationResult.reachedFinalization)
          votingWithoutValid
            .withValid(
              simulationResult.chosenValid,
              simulationResult.chosenValid.map(idx => valid(idx.toInt))
            )
            .leftMap(_.err)
        else votingWithoutValid.asRight

      voting.map(FinalizationResult(simulationResult.reachedFinalization, _))
    }

    private def verifySig(msg: EndorseBlock, pk: BlsPublicKey): Either[String, BlsSignature] = for {
      sig <- BlsSignature(msg.signature).leftMap(_.err)
      _   <- sig.verifyBasic(BlockEndorsement.mkMessage(msg.finalizedId, msg.finalizedHeight, msg.endorsedId), pk)
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
          aggregatedEndorsement = None,
          conflict = IndexedSeq.empty
        )
      )
    }
  }
}
