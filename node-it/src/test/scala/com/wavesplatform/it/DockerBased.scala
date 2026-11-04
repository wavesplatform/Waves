package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.Docker.DockerNode
import monix.eval.Coeval
import org.scalatest.{Args, BeforeAndAfterAll, Status, Suite}

trait DockerBased extends BeforeAndAfterAll {
  this: Suite & Nodes =>

  protected val dockerSingleton: Coeval[Docker] = Coeval.evalOnce(createDocker)
  final def docker: Docker                      = dockerSingleton()

  abstract override protected def runTest(testName: String, args: Args): Status = {
    def printThreadDump(): Unit = nodes.collect { case node: DockerNode =>
      docker.printThreadDump(node)
    }
    val r = super.runTest(testName, args)
    if (!r.succeeds() && Option(System.getenv("DISABLE_THREAD_DUMPS")).isEmpty) printThreadDump()
    r
  }

  // Override these rather than createDocker, so that options from different traits combine
  protected def suiteConfig: Config         = ConfigFactory.empty()
  protected def nodeExtensions: Seq[String] = Seq.empty
  // Only for suites that speak the binary protocol from the test JVM: nodes peer over the container network
  protected def publishNetworkPort: Boolean = false
  protected def createDocker: Docker        = Docker(getClass, suiteConfig, nodeExtensions, publishNetworkPort)
  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }
}
