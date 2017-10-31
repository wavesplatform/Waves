package com.wavesplatform.it

import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.Random

class ValidChainGenerationSuite extends FreeSpec with IntegrationNodesInitializationAndStopping
  with Matchers with TransferSending {

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
      _ <- waitForSameBlocksAt(baseHeight, 3.seconds)
    } yield (), 7.minutes)

    def waitForSameBlocksAt(height: Int, delay: FiniteDuration): Future[Boolean] = {
      traverse(nodes)(_.blockAt(height)).flatMap { blocks =>
        if (blocks.forall(_ == blocks.head)) Future.successful(true)
        else {
          Future {
            Thread.sleep(delay.toMillis)
          }.flatMap(_ => waitForSameBlocksAt(height, delay))
        }
      }
    }
  }
}
