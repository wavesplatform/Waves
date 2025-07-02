package com.wavesplatform.network

import com.google.common.cache.CacheBuilder
import com.typesafe.scalalogging.LazyLogging
import com.wavesplatform.account.PublicKey
import com.wavesplatform.state.Height
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import monix.execution.atomic.Atomic
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

import scala.jdk.CollectionConverters.*

object EndorseBlockSynchronizer extends LazyLogging {
  type EndorsersAt = (Height, Set[PublicKey])

  def start(
      maxActiveEndorsers: Int,
      last: Observable[EndorsersAt],
      endorsements: Observable[(Channel, EndorseBlock)],
      allChannels: DefaultChannelGroup
  )(implicit scheduler: Scheduler): Cancelable = {
    val known = CacheBuilder
      .newBuilder()
      .maximumSize(maxActiveEndorsers * 2) // 2 for valid and invalid
      .build[EndorseBlock, Object]

    def fit(x: EndorseBlock, cond: EndorsersAt): Boolean = {
      val (h, endorsers) = cond
      x.blockHeight == h && endorsers.contains(x.endorserPublicKey)
    }

    val current: Atomic[EndorsersAt] = Atomic((Height(0), Set.empty))
    last.foreach { e =>
      current.set(e)

      logger.trace(s"Invalidating known endorsements before ${e._1}")
      val stale = known.asMap().keySet().asScala.filterNot(fit(_, e))
      known.invalidateAll(stale.asJava)
    }

    val dummy = new Object()
    endorsements.foreach { case (ch, x) =>
      val suitableAndNew = x.verify() && fit(x, current.get()) && known.asMap().putIfAbsent(x, dummy) == null
      if (suitableAndNew) allChannels.broadcast(x, Some(ch))
    }
  }
}
