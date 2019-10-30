package com.wavesplatform.it.sync.grpc

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{ExternalNode, GrpcIntegrationSuiteWithThreeAddress, GrpcWaitForHeight, Node, NodeConfigs}
import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, FunSuite, Suite}

import scala.concurrent.ExecutionContext
import scala.collection.JavaConverters._

trait GrpcBaseTransactionSuiteLike
  extends GrpcWaitForHeight
  with GrpcIntegrationSuiteWithThreeAddress
  with BeforeAndAfterAll
  with NodesFromDocker { this: Suite =>

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

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
          .map(cfg => new ExternalNode(cfg.withFallback(defaultConfig).resolve()))
    }
  }

  protected override def beforeAll(): Unit = {
    theNodes.run
    super.beforeAll()
  }
}

abstract class GrpcBaseTransactionSuite extends FunSuite with GrpcBaseTransactionSuiteLike
