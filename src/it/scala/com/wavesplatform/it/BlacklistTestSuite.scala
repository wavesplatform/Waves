package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api.MultipleNodesApi
import com.wavesplatform.it.api.NodeApi.BlacklistedPeer
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.Await

class BlacklistTestSuite extends FreeSpec with Matchers with CancelAfterFailure with ReportingTestName
  with MultipleNodesApi {

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(2))
    .withDefault(3)
    .withSpecial(_.quorum(0))
    .build()

  private def primaryNode = nodes.last

  private def otherNodes = nodes.init

  "network should grow up to 10 blocks" in Await.result(primaryNode.waitForHeight(10), 3.minutes)

  "primary node should blacklist other nodes" in Await.result(
    for {
      _ <- traverse(otherNodes) { n => primaryNode.blacklist(n.nodeInfo.networkIpAddress, n.nodeInfo.hostNetworkPort) }
      expectedBlacklistedPeers = nodes.size - 1
      _ <- primaryNode.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers.size == $expectedBlacklistedPeers")(_.blacklistedPeers, _.size == expectedBlacklistedPeers, 1.second)
    } yield (),
    1.minute
  )

  "sleep while nodes are blocked" in Await.result(
    primaryNode.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers is empty")(_.blacklistedPeers, _.isEmpty, 5.second),
    primaryNode.settings.networkSettings.blackListResidenceTime + 10.seconds
  )

  "and sync again" in Await.result(
    for {
      baseHeight <- traverse(nodes)(_.height).map(_.max)
      _ <- waitForSameBlocksAt(nodes, 5.seconds, baseHeight + 5)
    } yield (),
    5.minutes
  )
}
