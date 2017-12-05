package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.util._
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class MicroblocksFeeTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure {

  import MicroblocksFeeTestSuite._

  private lazy val docker = Docker(getClass)
  private lazy val allNodes = docker.startNodes(Configs)

  private def notMiner = allNodes.head
  private def firstAddress = allNodes(1).address

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

  "fee distribution when NG activates" in {

    val f = for {
      height <- traverse(allNodes)(_.height).map(_.max)

      _ <- traverse(allNodes)(_.waitForHeight(microblockActivationHeight - 1))
      _ <- txRequestsGen(200, 2.waves)
      _ <- traverse(allNodes)(_.waitForHeight(microblockActivationHeight + 2))

      initialBalances <- notMiner.debugStateAt(microblockActivationHeight - 1) //100%

      balancesBeforeActivation <- notMiner.debugStateAt(microblockActivationHeight) // 100%
      blockBeforeActivation <- notMiner.blockAt(microblockActivationHeight)

      balancesOnActivation <- notMiner.debugStateAt(microblockActivationHeight + 1) // 40%
      blockOnActivation <- notMiner.blockAt(microblockActivationHeight + 1)

      balancesAfterActivation <- notMiner.debugStateAt(microblockActivationHeight + 2) // 60% of previous + 40% of current
      blockAfterActivation <- notMiner.blockAt(microblockActivationHeight + 2)
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

    import NodeConfigs.Default

    val NodesCount: Int = 3

    val microblockActivationHeight = 10
    private val miner = ConfigFactory.parseString(
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
         |   miner.quorum = 3
         |}
      """.stripMargin
    )
    private val notMiner = ConfigFactory.parseString(
      s"""waves {
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
         |   miner.enable = no
         |}
      """.stripMargin

    )


    val Configs: Seq[Config] = Seq(notMiner.withFallback(Default.head),
      notMiner.withFallback(Default(1)),
      miner.withFallback(Default(2)),
      miner.withFallback(Default(3)))

  }

}