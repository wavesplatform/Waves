package com.wavesplatform.network

import com.wavesplatform.network.MicroBlockSynchronizer.{MicroBlockSignature, cache}
import io.netty.channel.ChannelHandlerContext
import scala.collection.mutable.{Set => MSet}

import scala.concurrent.duration.FiniteDuration

class MicroBlockOwners(cacheTimeout: FiniteDuration) {
  private val knownOwners = cache[MicroBlockSignature, MSet[ChannelHandlerContext]](cacheTimeout)

  def add(totalSig: MicroBlockSignature, ownerCtx: ChannelHandlerContext): Unit = {
    knownOwners.get(totalSig, () => MSet.empty) += ownerCtx
  }

  def all(totalSig: MicroBlockSignature): Set[ChannelHandlerContext] = {
    Option(knownOwners.getIfPresent(totalSig)).getOrElse(MSet.empty).toSet
  }
}
