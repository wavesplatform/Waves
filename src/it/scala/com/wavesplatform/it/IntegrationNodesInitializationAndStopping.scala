package com.wavesplatform.it

import com.wavesplatform.it.NetworkSeparationTestSuite.NodesCount
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait IntegrationNodesInitializationAndStopping extends BeforeAndAfterAll { this: Suite =>
  def docker: Docker
  def nodes: Seq[Node]

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    Await.result(Future.traverse(nodes)(_.status), 1.minute)

    Await.result(
      for {
        count <- Future.traverse(nodes)(_.waitForPeers(NodesCount - 1))
      } yield count, 1.minute
    )
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }
}
