package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

trait IntegrationNodesInitializationAndStopping extends BeforeAndAfterAll with ScorexLogging with ReportingTestName {
  this: Suite =>
  protected lazy val docker: Docker = Docker(getClass)
  def nodes: Seq[Node]

  abstract override def beforeAll(): Unit = {
    super.beforeAll()
    nodes.foreach(_.status) // Initialize
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


