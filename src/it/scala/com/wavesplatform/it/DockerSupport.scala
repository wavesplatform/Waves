package com.wavesplatform.it

import org.scalatest.{BeforeAndAfterAll, Suite}

trait DockerSupport extends BeforeAndAfterAll { this: Suite =>
  val docker: Docker = Docker()

  override protected def afterAll(): Unit = docker.close()
}
