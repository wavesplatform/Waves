package com.wavesplatform.it

import org.scalatest._

class FirstSpec(docker: Docker) extends FreeSpec {
  "Starting several nodes" - {
    "works" in {
      val node1 = docker.startNode()
      val node2 = docker.startNode()
    }
  }
}
