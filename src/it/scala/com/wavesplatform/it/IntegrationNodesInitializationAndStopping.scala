package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging
import com.wavesplatform.it.api.AsyncHttpApi._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

trait IntegrationNodesInitializationAndStopping extends BeforeAndAfterAll with ScorexLogging
  with ReportingTestName with Nodes {
  this: Suite =>

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(traverse(nodes)(_.waitForHeight(2)), 1.minute)
  }

}
