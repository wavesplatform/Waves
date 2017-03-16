package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ValidChainGenerationSpec(docker: Docker) extends FreeSpec with Matchers with ScorexLogging {
  "Generate 30 blocks and synchronise" in {
    val fn1 = docker.startNode()
    val fn2 = docker.startNode()
    val fn3 = docker.startNode()

    val allNodes = Await.result(Future.sequence(Seq(fn1, fn2, fn3)), 30.seconds)


    log.info(allNodes
      .map(_.connectedPeers)
      .map(Await.result(_, Duration.Inf)).mkString("; "))
  }
}
