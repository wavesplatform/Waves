package com.wavesplatform.it

import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.Random

class ValidChainGenerationSuite extends FreeSpec with IntegrationNodesInitializationAndStopping
  with Matchers with TransferSending {

  override val docker = Docker(getClass)
  override val nodes: Seq[Node] = docker.startNodes(NodeConfigs.default(3, 1))

  "Generate more blocks and resynchronise after rollback" - {
    "1 of N" in test(1)
    "N-1 of N" in test(nodes.size - 1)

    def test(n: Int): Unit = {
      val initialHeight = result(for {
        height <- traverse(nodes)(_.height).map(_.max)
        newHeight = height + 5
        _ <- traverse(nodes)(_.waitForHeight(newHeight))
      } yield newHeight, 5.minutes)

      val targetHeight = initialHeight + 2
      val rollbackNodes = Random.shuffle(nodes).take(n)

      rollbackNodes.foreach(_.rollback(1))
      val synchronizedBlocks = result(for {
        _ <- traverse(nodes)(_.waitForHeight(targetHeight))
        blocks <- traverse(nodes)(_.blockAt(initialHeight))
      } yield blocks, 5.minutes)

      all(synchronizedBlocks) shouldEqual synchronizedBlocks.head
    }
  }
}
