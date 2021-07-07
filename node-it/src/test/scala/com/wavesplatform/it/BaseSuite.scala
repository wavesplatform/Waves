package com.wavesplatform.it

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.it.transactions.NodesFromDocker
import monix.eval.Coeval
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

trait BaseSuite
    extends ReportingTestName
    with NodesFromDocker
    with matchers.should.Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach { this: TestSuite with Nodes =>
  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

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
