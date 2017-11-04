package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

trait IntegrationNodesInitializationAndStopping extends BeforeAndAfterAll with ScorexLogging with ReportingTestName {
  this: Suite =>
  def docker: Docker

  def nodes: Seq[Node]

  abstract override def beforeAll(): Unit = {
    super.beforeAll()
    log.debug("Waiting for nodes to start")
    Await.result(Future.traverse(nodes)(_.status), 1.minute)
    log.debug("Waiting for nodes to connect")
    Await.result(
      for {
        count <- Future.traverse(nodes)(_.waitForPeers(nodes.size - 1))
      } yield count, 1.minute
    )
  }

  abstract override def afterAll(): Unit = {
    super.afterAll()
    ensureNoDeadlock()
    docker.close()
  }

  private def ensureNoDeadlock() = {
    Await.result(Future.traverse(nodes)(_.height), 7.seconds)
  }

}


