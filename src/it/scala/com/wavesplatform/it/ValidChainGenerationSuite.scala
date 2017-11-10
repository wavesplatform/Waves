package com.wavesplatform.it

import com.wavesplatform.it.api._
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.Random

class ValidChainGenerationSuite extends FreeSpec with IntegrationNodesInitializationAndStopping
  with Matchers with TransferSending with MultipleNodesApi {

  override lazy val nodes: Seq[Node] = docker.startNodes(NodeConfigs.forTest(3, 1 -> "waves.miner.enable = no"))

  "Generate more blocks and resynchronise after rollback" - {
    "1 of N" in test(1)
    "N-1 of N" in test(nodes.size - 1)

    def test(n: Int): Unit = result(for {
      height <- traverse(nodes)(_.height).map(_.max)
      baseHeight = height + 5
      _ <- traverse(nodes)(_.waitForHeight(baseHeight))

      rollbackNodes = Random.shuffle(nodes).take(n)
      _ <- traverse(rollbackNodes)(_.rollback(1))
      _ <- waitForSameBlocksAt(nodes, 5.seconds, baseHeight)
    } yield (), 7.minutes)
  }
}
