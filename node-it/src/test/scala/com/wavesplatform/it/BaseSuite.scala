package com.wavesplatform.it

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.transactions.NodesFromDocker
import monix.eval.Coeval
import org.scalatest._

import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext

class BaseSuite
    extends FreeSpec
    with ReportingTestName
    with NodesFromDocker
    with Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach {
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

  // protected because https://github.com/sbt/zinc/issues/292
  protected val theNodes: Coeval[Seq[Node]] = Coeval.evalOnce {
    Option(System.getProperty("waves.it.config.file")) match {
      case None => dockerNodes()
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
    theNodes.run()
    super.beforeAll()
  }
}
