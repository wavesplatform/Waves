package com.wavesplatform.network

import com.google.common.cache.CacheBuilder
import com.typesafe.scalalogging.LazyLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.FinalizationVoting
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.BlsPublicKey
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import monix.execution.atomic.{Atomic, AtomicAny}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

import scala.jdk.CollectionConverters.*

trait EndorsementStorage {
  /*
 should store something like:
case class FinalizationVoting(
    endorsers: Seq[BlsPublicKey],
    aggregatedEndorsement: BlsSignature,
    invalid: Seq[BlockEndorsement.Conflict]
) {
??
   */

  // TODO: Preserves the order of insertion
  def tryAdd(msg: EndorseBlock): Boolean
  def switch(endorsedBlockId: BlockId, endorsers: Set[BlsPublicKey]): Unit // TODO: use in appender when changed height
  def takeAndClear(endorsedBlockId: BlockId): Option[FinalizationVoting]
}

object EndorsementStorage {
  val Disabled: EndorsementStorage = new EndorsementStorage {
    override def tryAdd(msg: EndorseBlock): Boolean                                   = false
    override def switch(endorsedBlockId: BlockId, endorsers: Set[BlsPublicKey]): Unit = {}
    override def takeAndClear(endorsedBlockId: BlockId): Option[FinalizationVoting]   = None
  }

  private case class Data(endorsedBlockId: BlockId, expectedEndorsers: Set[BlsPublicKey], voting: Option[FinalizationVoting])

  def apply(
      maxEndorsers: Int
  ): EndorsementStorage = new EndorsementStorage with ScorexLogging {
    private val current: Atomic[Data] = AtomicAny(Data(ByteStr.empty, Set.empty, None))

    private val known = CacheBuilder
      .newBuilder()
      .maximumSize(maxEndorsers * 2) // 2 for valid and conflict
      .build[EndorseBlock, Object]

    private val dummy = new Object()

    override def tryAdd(msg: EndorseBlock): Boolean = {
      // TODO: getAndSet, update voting, remove expected
      val c = current.get()
      val r = fit(msg, c.endorsedBlockId, c.expectedEndorsers)
      if (r) {
        known.put(msg, dummy)
      }
      r
    }

    override def switch(endorsedBlockId: BlockId, endorsers: Set[BlsPublicKey]): Unit = {
      current.set(Data(endorsedBlockId, endorsers, None))

      logger.trace(s"Invalidating known endorsements for $endorsedBlockId")
      val stale = known.asMap().keySet().asScala.filterNot(fit(_, endorsedBlockId, endorsers))
      known.invalidateAll(stale.asJava)
    }

    override def takeAndClear(endorsedBlockId: BlockId): Option[FinalizationVoting] = {
      val voting = current.getAndTransform { x => Data(x.endorsedBlockId, x.expectedEndorsers, None) }.voting

      // Remove known:
      // val endorsers = voting.fold(Seq.empty)(_.endorsers).toSet
      // val k = known.asMap().keySet().asScala.filter(x => endorsers.contains(x.endorserPublicKey))
      // known.invalidateAll(k.asJava)

      voting
    }

    private def fit(x: EndorseBlock, endorsedBlockId: BlockId, endorsers: Set[BlsPublicKey]): Boolean =
      x.blockId == endorsedBlockId && endorsers.contains(x.endorserPublicKey)
  }
}

object EndorseBlockSynchronizer extends LazyLogging {
  def start(
      storage: EndorsementStorage,
      lastEndorsers: Observable[(BlockId, Set[BlsPublicKey])],
      receivingEndorsements: Observable[(Channel, EndorseBlock)],
      allChannels: DefaultChannelGroup,
      scheduler: Scheduler
  ): Cancelable = {
    // TODO: move outside
    lastEndorsers.foreach(Function.tupled(storage.switch))(using scheduler)

    receivingEndorsements.foreach { case (ch, x) =>
      if (storage.tryAdd(x)) allChannels.broadcast(x, Some(ch))
    }(using scheduler)
  }
}
