package com.wavesplatform.it.transactions

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, FunSuite, Suite}

import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext

trait BaseTransactionSuiteLike extends WaitForHeight2 with IntegrationSuiteWithThreeAddresses with BeforeAndAfterAll with NodesFromDocker {
  this: Suite =>

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  override def miner: Node = nodes.head

  private var istRunning = false

  // protected because https://github.com/sbt/zinc/issues/292
  protected lazy val theNodes: Coeval[Seq[Node]] = Coeval.evalOnce {
    require(istRunning, "Do not attempt to access node instances from suite constructors")
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
    istRunning = true
    theNodes.run()
    super.beforeAll()
  }
}

abstract class BaseTransactionSuite extends FunSuite with BaseTransactionSuiteLike
