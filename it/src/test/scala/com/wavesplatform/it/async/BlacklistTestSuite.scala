package com.wavesplatform.it.async

import com.typesafe.config.Config
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class BlacklistTestSuite extends FreeSpec with Matchers with CancelAfterFailure with ReportingTestName with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(2))
      .withDefault(3)
      .withSpecial(_.quorum(0))
      .buildNonConflicting()

  private def primaryNode = dockerNodes().last

  private def otherNodes = dockerNodes().init

  "network should grow up to 10 blocks" in Await.result(primaryNode.waitForHeight(10), 3.minutes)

  "primary node should blacklist other nodes" in Await.result(
    for {
      _ <- traverse(otherNodes) { n =>
        primaryNode.blacklist(n.containerNetworkAddress)
      }
      expectedBlacklistedPeers = nodes.size - 1
      _ <- primaryNode.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers.size == $expectedBlacklistedPeers")(
        _.blacklistedPeers,
        _.lengthCompare(expectedBlacklistedPeers) == 0,
        1.second)
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
      _          <- nodes.waitForSameBlockHeadesAt(baseHeight + 5)
    } yield (),
    5.minutes
  )
}
