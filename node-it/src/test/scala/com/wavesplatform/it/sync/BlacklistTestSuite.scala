package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import org.scalatest._
import scala.concurrent.duration._

class BlacklistTestSuite extends FreeSpec with Matchers with CancelAfterFailure with ReportingTestName with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(1))
      .withDefault(2)
      .withSpecial(_.quorum(0))
      .buildNonConflicting()

  private def primaryNode = dockerNodes().last

  private def otherNodes = dockerNodes().init

  "primary node should blacklist other nodes" in {
    otherNodes.foreach(n => primaryNode.blacklist(n.containerNetworkAddress))

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
