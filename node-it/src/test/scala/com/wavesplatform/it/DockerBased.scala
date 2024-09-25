package com.wavesplatform.it

import com.wavesplatform.it.Docker.DockerNode
import monix.eval.Coeval
import org.scalatest.{Args, BeforeAndAfterAll, Status, Suite}

trait DockerBased extends BeforeAndAfterAll {
  this: Suite with Nodes =>

  protected val dockerSingleton: Coeval[Docker] = Coeval.evalOnce(createDocker)
  final def docker: Docker                      = dockerSingleton()

  abstract override protected def runTest(testName: String, args: Args): Status = {
    def printThreadDump(): Unit = nodes.collect { case node: DockerNode =>
      docker.printThreadDump(node)
    }
    val r = super.runTest(testName, args)
    if (!r.succeeds()) printThreadDump()
    r
  }

  protected def createDocker: Docker = Docker(getClass)
  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }
}
