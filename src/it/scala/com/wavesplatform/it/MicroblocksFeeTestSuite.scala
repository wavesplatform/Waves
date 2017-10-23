package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.it.util._
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.Random


class MicroblocksFeeTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure {

  import MicroblocksFeeTestSuite._

  private val docker = Docker(getClass)
  private val allNodes = Configs.map(docker.startNode)


  private val notMiner = allNodes.head
  private val firstAddress = allNodes(1).address


  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(allNodes)(_.waitForPeers(NodesCount - 1)), 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }


  private def txRequestsGen(n: Int, fee: Long): Future[Unit] = {
    val parallelRequests = 1

    def requests(n: Int): Future[Unit] = Future
      .sequence {
        //Not mining node sends transfer transactions to another not mining node
        //Mining nodes collect fee
        (1 to n).map { _ => notMiner.transfer(notMiner.address, firstAddress, (1 + Random.nextInt(10)).waves, fee) }
      }
      .map(_ => ())

    val steps = (1 to n)
      .sliding(parallelRequests, parallelRequests)
      .map(_.size)

    steps.foldLeft(Future.successful(())) { (r, numRequests) => r.flatMap(_ => requests(numRequests)) }
  }

  "microblockheightactivation  and fee" in {
    val mActivationHeight = microblockActivationHeight + 1

    val f = for {
      height <- traverse(allNodes)(_.height).map(_.max)

      _ <- traverse(allNodes)(_.waitForHeight(mActivationHeight - 2))
      _ <- txRequestsGen(200, 2.waves)
      _ <- traverse(allNodes)(_.waitForHeight(mActivationHeight + 1))

      initialBalances <- notMiner.debugStateAt(mActivationHeight - 2) //100%

      balancesBeforeActivation <- notMiner.debugStateAt(mActivationHeight - 1) // 100%
      blockBeforeActivation <- notMiner.blockAt(mActivationHeight - 1)

      balancesOnActivation <- notMiner.debugStateAt(mActivationHeight) // 40%
      blockOnActivation <- notMiner.blockAt(mActivationHeight)

      balancesAfterActivation <- notMiner.debugStateAt(mActivationHeight + 1) // 60% of previous + 40% of current
      blockAfterActivation <- notMiner.blockAt(mActivationHeight + 1)
    } yield {

      balancesBeforeActivation(blockBeforeActivation.generator) shouldBe {
        initialBalances(blockBeforeActivation.generator) + blockBeforeActivation.fee
      }

      balancesOnActivation(blockOnActivation.generator) shouldBe {
        balancesBeforeActivation(blockOnActivation.generator) + blockOnActivation.fee * 0.4
      }

      balancesAfterActivation(blockAfterActivation.generator) shouldBe {
        balancesOnActivation(blockAfterActivation.generator) +
          blockOnActivation.fee * 0.6 + blockAfterActivation.fee * 0.4
      }
    }

    Await.result(f, 2.minute)
  }

  object MicroblocksFeeTestSuite {

    private val dockerConfigs = Docker.NodeConfigs.getConfigList("nodes").asScala

    val NodesCount: Int = 3

    val microblockActivationHeight = 10
    private val supportedNodes = ConfigFactory.parseString(
      s"""
         |waves {
         |   blockchain {
         |     custom {
         |        functionality{
         |          pre-activated-features = {2=$microblockActivationHeight}
         |        }
         |        genesis {
         |          signature: "gC84PYfvJRdLpUKDXNddTcWmH3wWhhKD4W9d2Z1HY46xkvgAdqoksknXHKzCBe2PEhzmDW49VKxfWeyzoMB4LKi"
         |          transactions = [
         |            {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 250000000000000},
         |            {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 250000000000000},
         |            {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 250000000000000},
         |            {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 250000000000000}
         |          ]
         |       }
         |      }
         |   }
         |}
      """.stripMargin
    )
    private val notMiner = ConfigFactory.parseString(
      s"""
         |waves.miner.enable=no
         |waves {
         |   blockchain {
         |     custom {
         |        functionality{
         |          pre-activated-features = {2=$microblockActivationHeight}
         |        }
         |        genesis {
         |          signature: "gC84PYfvJRdLpUKDXNddTcWmH3wWhhKD4W9d2Z1HY46xkvgAdqoksknXHKzCBe2PEhzmDW49VKxfWeyzoMB4LKi"
         |          transactions = [
         |            {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 250000000000000},
         |            {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 250000000000000},
         |            {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 250000000000000},
         |            {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 250000000000000}
         |          ]
         |       }
         |      }
         |   }
         |}
      """.stripMargin

    )


    val Configs: Seq[Config] = Seq(notMiner.withFallback(dockerConfigs.head)) ++
      Seq(notMiner.withFallback(dockerConfigs(1))) ++
      Seq(supportedNodes.withFallback(dockerConfigs(2))) ++
      Seq(supportedNodes.withFallback(dockerConfigs(3)))

  }

}