package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

trait IntegrationNodesInitializationAndStopping extends BeforeAndAfterAll with ScorexLogging
  with ReportingTestName with AsyncNodes {
  this: Suite =>

  abstract override def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(traverse(nodes)(_.waitForHeight(2)), 1.minute)
  }

}
