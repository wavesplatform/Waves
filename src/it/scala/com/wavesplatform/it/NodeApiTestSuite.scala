package com.wavesplatform.it

import com.wavesplatform.it.NodeApiTestSuite._
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class NodeApiTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with ScorexLogging {

  private lazy val docker = Docker(getClass)

  "/node/status should report status" in {
    val node = docker.startNode(NodeConfig)
    val f = for {
      status <- node.status
      _ = assert(status.blockchainHeight >= status.stateHeight)
    } yield succeed

    Await.ready(f, 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }
}

private object NodeApiTestSuite {

  private val NodeConfig = NodeConfigs.newBuilder.withDefault(1).build().head

}
