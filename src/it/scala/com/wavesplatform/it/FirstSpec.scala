package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.duration._

class FirstSpec(docker: Docker) extends FreeSpec with ScorexLogging {

  "Starting several nodes" - {
    "works" in {
      info("starting nodes")
      val node1 = docker.startNode()
      val node2 = docker.startNode()

      info("waiting for node1 to become available")
      log.info(s"${Await.result(node1, Duration.Inf)}")
    }
  }
}
