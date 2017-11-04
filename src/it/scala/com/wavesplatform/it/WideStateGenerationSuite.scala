package com.wavesplatform.it

import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class WideStateGenerationSuite extends FreeSpec with IntegrationNodesInitializationAndStopping
  with Matchers with TransferSending {

  override val docker = Docker(getClass)
  override val nodes: Seq[Node] = docker.startNodes(NodeConfigs.default(3, 1))

  private val requestsCount = 10000

  "Generate a lot of transactions and synchronise" in {
    val targetBlocks = result(for {
      b <- traverse(nodes)(balanceForNode).map(_.toMap)
      _ <- processRequests(generateTransfersToRandomAddresses(requestsCount / 2, b) ++ generateTransfersBetweenAccounts(requestsCount / 2, b))

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 30))

      blocks <- traverse(nodes)(_.blockAt(height + 10))
    } yield blocks.map(_.signature), 10.minutes)

    all(targetBlocks) shouldEqual targetBlocks.head
  }
}
