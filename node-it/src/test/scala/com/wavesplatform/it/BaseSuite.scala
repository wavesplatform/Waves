package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.test.TestUtils
import monix.eval.Coeval
import org.scalatest.*

import java.io.File
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*

trait BaseSuite
    extends ReportingTestName
    with NodesFromDocker
    with TestUtils
    with matchers.should.Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach { this: TestSuite & Nodes =>
  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  import com.wavesplatform.it.NodeConfigs.*
  override protected def nodeConfigs: Seq[Config] = Seq(
    BiggestMiner.quorum(0),
    Default.head.notMiner
  )

  def miner: Node            = nodes.head
  def notMiner: Node         = nodes.last
  protected def sender: Node = miner

  private var isRunning = false

  // protected because https://github.com/sbt/zinc/issues/292
  protected val theNodes: Coeval[Seq[Node]] = Coeval.evalOnce {
    require(isRunning, "Do not attempt to access node instances from suite constructors")
    Option(System.getProperty("waves.it.config.file")) match {
      case None =>
        AddressScheme.current = new AddressScheme {
          override val chainId: Byte = 'I'
        }
        dockerNodes()
      case Some(filePath) =>
        val defaultConfig = ConfigFactory.load()
        ConfigFactory
          .parseFile(new File(filePath))
          .getConfigList("nodes")
          .asScala
          .toSeq
          .map(cfg => new ExternalNode(cfg.withFallback(defaultConfig).resolve()))
    }
  }

  override protected def nodes: Seq[Node] = theNodes()

  protected override def beforeAll(): Unit = {
    isRunning = true
    theNodes.run()
    super.beforeAll()
  }
}

abstract class BaseFreeSpec extends freespec.AnyFreeSpec with BaseSuite

abstract class BaseFunSuite extends funsuite.AnyFunSuite with BaseSuite
