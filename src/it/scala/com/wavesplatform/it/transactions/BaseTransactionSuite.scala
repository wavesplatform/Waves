package com.wavesplatform.it.transactions

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.Node
import monix.eval.Coeval
import org.scalatest.FunSuite

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext

abstract class BaseTransactionSuite extends FunSuite with WaitForHeight2
  with IntegrationSuiteWithThreeAddresses with NodesFromDocker {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(3))
    .withDefault(3)
    .withSpecial(_.nonMiner)
    .buildNonConflicting()

  override def notMiner: Node = nodes.last

  private val theNodes = Coeval.evalOnce {
    Option(System.getProperty("waves.it.config.file")) match {
      case None => nodesSingleton()
      case Some(filePath) =>
        val defaultConfig = ConfigFactory.load()
        ConfigFactory
          .parseFile(new File(filePath))
          .getConfigList("nodes")
          .asScala
          .map(cfg => NodeImpl(cfg.withFallback(defaultConfig).resolve()))
    }
  }
  override protected def nodes: Seq[Node] = theNodes()
}
