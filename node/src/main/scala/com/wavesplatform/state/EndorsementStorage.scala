package com.wavesplatform.state

import cats.syntax.either.*
import cats.syntax.option.*
import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter.SimulationResult
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

  case class EndorsementFilter(
      miner: Option[GeneratorIndex],
      finalizedId: BlockId,
      finalizedHeight: Height,
      endorsedId: BlockId,
      endorsers: IndexedSeq[(BlsPublicKey, Long)],
      conflict: Set[GeneratorIndex]
  ) {
    private val minerBalance        = miner.fold(0L)(i => endorsers(i.toInt)._2)
    private val doubledTotalBalance = endorsers.foldLeft(BigInt(0L)) { case (r, (_, b)) => r + b } * 2

    override def toString: String =
      s"EndorsementFilter(${miner.fold("")(i => s"m=$i, ")}fid=$finalizedId, fh=$finalizedHeight, eid=$endorsedId, e={${endorsers.mkString(", ")}})"

    def sameVoting(other: EndorsementFilter): Boolean =
      finalizedId == other.finalizedId && finalizedHeight == other.finalizedHeight && endorsedId == other.endorsedId

    def simulate(voterIndexes: Iterable[Int]): SimulationResult = {
      type Item = (idx: GeneratorIndex, blsPk: BlsPublicKey, balance: Long)
      val lifted = endorsers.lift
      val items = for {
        idx              <- voterIndexes.view
        (blsPk, balance) <- lifted(idx)
      } yield (GeneratorIndex(idx), blsPk, balance): Item

      val richest = mutable.PriorityQueue.empty[Item](using Ordering.by(-_.balance))
      richest.addAll(items)

      var endorserIndexes = Vector.empty[GeneratorIndex]
      var endorsedBalance = BigInt(minerBalance)
      var complete        = false
      while (richest.nonEmpty && !complete) {
        val x = richest.dequeue()
        endorserIndexes = endorserIndexes.appended(x.idx)
        endorsedBalance += x.balance
        complete = endorsedBalance * 3 >= doubledTotalBalance // Same as endorsedBalance >= totalBalance * 2 / 3, but with precision
      }

      SimulationResult(complete, endorserIndexes)
    }
  }

  object EndorsementFilter {
    case class SimulationResult(complete: Boolean = false, chosenValid: IndexedSeq[GeneratorIndex] = Vector.empty)
  }

  object Disabled extends EndorsementStorage {
    override def tryAdd(msg: EndorseBlock): Either[String, Boolean]                  = true.asRight
    override def startVoting(filter: EndorsementFilter): Boolean                     = false
    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = None
  }

  class InMemory(blockAtHeight: (BlockId, Height) => Boolean) extends EndorsementStorage with StrictLogging {
    private var currentFilter = none[EndorsementFilter]

    private val sharedWithNeighbors     = mutable.HashSet.empty[EndorseBlock]
    private val processedValidEndorsers = mutable.HashSet.empty[GeneratorIndex]

    private var valid    = immutable.IntMap.empty[BlsSignature.NonEmpty]
    private var conflict = immutable.IntMap.empty[BlockEndorsement.Conflict]

    private case class ResultType(simulation: SimulationResult, voting: FinalizationVoting) // TODO: move
    private var latestResult = ResultType(SimulationResult(), FinalizationVoting())
    private var hasChanges   = true

    private val monitor            = new Object()
    private def synced[T](f: => T) = monitor.synchronized(f)

    override def tryAdd(msg: EndorseBlock): Either[String, Boolean] = synced {
      for {
        filter        <- currentFilter.toRight("Voting hasn't started")
        _             <- Either.raiseWhen(msg.finalizedHeight > filter.finalizedHeight)(s"Expected finalized height <= ${filter.finalizedHeight}")
        _             <- Either.raiseWhen(msg.endorserIndex >= filter.endorsers.size)(s"There are only ${filter.endorsers.size} endorsers")
        endorserIndex <- GeneratorIndex.checked(msg.endorserIndex).toRight(s"Invalid endorser index: ${msg.endorserIndex}")
        (endorserPk, _) = filter.endorsers(msg.endorserIndex)
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
              BlockEndorsement.Conflict(GeneratorIndex(msg.endorserIndex), msg.finalizedId, sig)
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

    // TODO: if not activated
    override def startVoting(filter: EndorsementFilter): Boolean = synced {
      val isNewVoting = !currentFilter.exists(_.sameVoting(filter))
      if (isNewVoting) {
        sharedWithNeighbors.clear()
        valid = valid.empty
        conflict = conflict.empty
        hasChanges = true

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
      (for {
        currentFilter <- currentFilter
        if currentFilter.endorsedId == endorsedId && hasChanges
      } yield {
        hasChanges = false

        val moreConflict = conflict.size > latestResult.voting.conflict.size
        val moreValid    = valid.size > latestResult.voting.valid.size
        if (moreConflict || !latestResult.simulation.complete && moreValid) {
          val simulation = currentFilter.simulate(valid.keys)

          val origResult = latestResult
          latestResult = ResultType(simulation, createVoting(currentFilter, simulation))

          Option.when(moreConflict || latestResult.simulation.complete != origResult.simulation.complete) {
            latestResult.voting
          }
        } else none
      }).flatten
    }

    private def createVoting(currentFilter: EndorsementFilter, simulationResult: SimulationResult): FinalizationVoting = {
      val init = FinalizationVoting(finalizedHeight = currentFilter.finalizedHeight, conflict = conflict.values.toIndexedSeq)
      simulationResult.chosenValid.foldLeft(init) { case (r, idx) =>
        r.withValid(idx, valid(idx.toInt))
      }
    }

    private def verifySig(msg: EndorseBlock, pk: BlsPublicKey): Option[BlsSignature.NonEmpty] =
      for {
        sig <- BlsSignature(msg.signature).toOption
        _   <- Option.when(pk.verify(BlockEndorsement.mkMessage(msg.finalizedId, msg.finalizedHeight, msg.endorsedId), sig))(sig)
      } yield sig
  }
}
