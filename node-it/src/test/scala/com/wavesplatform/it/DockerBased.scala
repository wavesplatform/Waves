package com.wavesplatform.it

import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, Suite}

trait DockerBased extends BeforeAndAfterAll {
  this: Suite =>
  protected val dockerSingleton: Coeval[Docker] = Coeval.evalOnce(createDocker)
  final def docker: Docker            = dockerSingleton()

  protected def createDocker: Docker = Docker(getClass)
  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }
}
