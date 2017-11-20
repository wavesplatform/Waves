package com.wavesplatform.network

import com.wavesplatform.network.MicroBlockSynchronizer.{MicroBlockSignature, cache}
import io.netty.channel.{Channel}

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration.FiniteDuration

class MicroBlockOwners(cacheTimeout: FiniteDuration) {
  private val knownOwners = cache[MicroBlockSignature, MSet[Channel]](cacheTimeout)

  def add(totalSig: MicroBlockSignature, channel: Channel): Unit = {
    knownOwners.get(totalSig, () => MSet.empty) += channel
  }

  def all(totalSig: MicroBlockSignature): Set[Channel] = {
    Option(knownOwners.getIfPresent(totalSig)).getOrElse(MSet.empty).toSet
  }
}
