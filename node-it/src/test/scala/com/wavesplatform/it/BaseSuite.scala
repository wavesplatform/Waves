package com.wavesplatform.it

import java.io.File

import AccountRestApi._
import NodeRestApi._
import NodesRestApi._

import com.typesafe.config.{Config, ConfigFactory}
import monix.eval.Coeval
import org.scalatest._

import scala.collection.JavaConverters._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class BaseSuite
  extends FreeSpec
    with NodesFromDocker
    with Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  //TODO support test run on existing or external nodes
  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  def miner: Node            = nodes.head
  def notMiner: Node         = nodes.last

  protected lazy val faucet: Account = Account(nodes, seedPhrase = Some(notMiner.settings.walletSettings.seed.get.toString))

  protected lazy val Seq(alice, bob, carol) = createAccounts(100.waves, 100.waves, 100.waves)

  //TODO актуально?
  //protected because https://github.com/sbt/zinc/issues/292
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

  override protected def nodes: Seq[Node] = theNodes()

  protected def createAccount(balance: Long): Account = {
    val account = Account(nodes)
    faucet.api.transfer(account, balance)
    //TODO log.debug(address, balance, seed)
    account
  }

  protected def createAccounts(balances: Long*): Seq[Account] = {
    val tasks = balances.map { balance =>
      Future {
        createAccount(balance)
      }
    }
    val r = Future.sequence(tasks)
    Await.result(r,  10.seconds)
  }

  //TODO проверить
  protected override def beforeAll(): Unit = {
    theNodes.run
    super.beforeAll()
  }

}
