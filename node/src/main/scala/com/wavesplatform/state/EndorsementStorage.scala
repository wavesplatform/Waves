package com.wavesplatform.state

import cats.syntax.either.*
import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter
import com.wavesplatform.state.Height

import scala.collection.mutable

// TODO: .switch: use in appender when changed height
trait EndorsementStorage {

  /** Add a vote, preserves the order of voting.
    * @return true, if it can be shared with neighbours
    */
  def tryAddVote(msg: EndorseBlock): Either[String, Boolean]

  /** @return true if it is a new voting */
  def startVoting(filter: EndorsementFilter): Boolean

  /** Returns a voting results at this time and resets all except an aggregated voting signature. */
  def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting]
}

object EndorsementStorage {

  /** @param miner
    *   True if this node is a miner
    */
  case class EndorsementFilter(
      miner: Boolean,
      finalizedId: BlockId,
      finalizedHeight: Height,
      endorsedId: BlockId,
      expectedEndorsers: Seq[BlsPublicKey]
  ) {
    override def toString: String =
      s"EndorsementFilter(m=$miner, fid=$finalizedId, fh=$finalizedHeight, eid=$endorsedId, e={${expectedEndorsers.mkString(", ")}})"

    def sameVoting(other: EndorsementFilter): Boolean =
      finalizedId == other.finalizedId && finalizedHeight == other.finalizedHeight && endorsedId == other.endorsedId
  }

  object Disabled extends EndorsementStorage {
    override def tryAddVote(msg: EndorseBlock): Either[String, Boolean]              = true.asRight
    override def startVoting(filter: EndorsementFilter): Boolean                     = false
    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = None
  }

  class InMemory extends EndorsementStorage with StrictLogging {
    private var currentFilter = Option.empty[EndorsementFilter]
    private var currentVoting = FinalizationVoting()
    private val processed     = mutable.HashSet.empty[EndorseBlock]

    private val monitor            = new Object()
    private def synced[T](f: => T) = monitor.synchronized(f)

    override def tryAddVote(msg: EndorseBlock): Either[String, Boolean] = synced {
      for {
        filter <- currentFilter.toRight("Voting hasn't started")
        _      <- Either.raiseUnless(msg.finalizedHeight == filter.finalizedHeight)(s"Expected finalized height ${filter.finalizedHeight}")
        _      <- Either.raiseWhen(msg.endorserIndex < 0)(s"Invalid endorser index: ${msg.endorserIndex}")
        _      <- Either.raiseWhen(msg.endorserIndex >= filter.expectedEndorsers.size)(s"There are only ${filter.expectedEndorsers.size} endorsers")
        endorserPk = filter.expectedEndorsers(msg.endorserIndex)
        sig <- verifySig(msg, endorserPk).toRight("Invalid signature")
        _   <- Either.raiseUnless(msg.endorsedId == filter.endorsedId)(s"Expected block ${filter.endorsedId}") // Could be a switch to a better branch
      } yield
        if (processed.contains(msg)) false
        else {
          // TODO: Do we need this if not mine now?
          val isConsistent = msg.finalizedId == filter.finalizedId
          currentVoting =
            if (isConsistent) currentVoting.withValid(msg.endorserIndex, sig)
            else currentVoting.withConflict(toConflict(msg, sig))

          processed += msg

          !filter.miner // Share with neighbours only if this node isn't a miner
        }
    }

    private def verifySig(msg: EndorseBlock, pk: BlsPublicKey): Option[BlsSignature.NonEmpty] =
      for {
        sig <- BlsSignature(msg.signature).toOption
        _   <- Option.when(pk.verify(BlockEndorsement.mkMessage(msg.finalizedId, msg.finalizedHeight, msg.endorsedId), sig))(sig)
      } yield sig

    private def toConflict(msg: EndorseBlock, verifiedSig: BlsSignature.NonEmpty): BlockEndorsement.Conflict =
      BlockEndorsement.Conflict(msg.endorserIndex, msg.finalizedId, verifiedSig)

    // TODO: if not activated
    override def startVoting(filter: EndorsementFilter): Boolean = synced {
      val isNewVoting = !currentFilter.exists(_.sameVoting(filter))
      if (isNewVoting) {
        currentVoting = FinalizationVoting()
        processed.clear()

        currentFilter = if (filter.expectedEndorsers.isEmpty) {
          logger.info("No committed generators, don't collect endorsements")
          None
        } else {
          logger.info(s"Started voting with $filter")
          Some(filter)
        }
      } else logger.trace(s"Same voting: current=$currentVoting vs new=$filter")
      isNewVoting
    }

    override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = synced {
      for {
        currentFilter <- currentFilter
        if currentFilter.endorsedId == endorsedId && currentVoting.hasUpdates
      } yield {
        val r = currentVoting
        currentVoting = currentVoting.copy(endorserIndexes = Seq.empty, conflict = Seq.empty)
        r
      }
    }
  }
}
