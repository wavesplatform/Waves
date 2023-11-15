package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.it.api.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}

import scala.concurrent.duration.*

class BlacklistTestSuite extends BaseFreeSpec {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(1))
      .withDefault(2)
      .buildNonConflicting()

  private def primaryNode = dockerNodes().last

  private def otherNodes = dockerNodes().init

  "primary node should blacklist other nodes" in {
    otherNodes.foreach(n => primaryNode.blacklist(n.networkAddress))

    val expectedBlacklistedPeers = nodes.size - 1

    primaryNode.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers.size == $expectedBlacklistedPeers")(
      _ => primaryNode.blacklistedPeers,
      _.lengthCompare(expectedBlacklistedPeers) == 0,
      1.second
    )
  }

  "sleep while nodes are blocked" in {
    primaryNode.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers is empty")(_.blacklistedPeers, _.isEmpty, 5.second)
  }

  "and sync again" in {
    val baseHeight = nodes.map(_.height).max
    nodes.waitForSameBlockHeadersAt(baseHeight + 5)
  }

}
