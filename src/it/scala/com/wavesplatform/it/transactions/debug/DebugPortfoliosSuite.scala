package com.wavesplatform.it.transactions.debug

import com.wavesplatform.it.util._
import com.wavesplatform.it._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class DebugPortfoliosSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses with TableDrivenPropertyChecks {

  override val docker = Docker(getClass)
  override val nodes: Seq[Node] = NodeConfigs.default(3, 1).map(docker.startNode)

  override val allNodes: Seq[Node] = nodes
  override val notMiner: Node = allNodes.last

  test("getting a balance considering pessimistic transactions from UTX pool - changed after UTX") {
    val f = for {
      portfolioBefore <- sender.debugPortfoliosFor(firstAddress, considerUnspent = true)
      utxSizeBefore <- sender.utxSize

      _ <- sender.payment(firstAddress, secondAddress, 5.waves, fee = 5.waves)
      _ <- sender.payment(secondAddress, firstAddress, 7.waves, 5.waves)
      _ <- sender.waitForUtxIncreased(utxSizeBefore)

      portfolioAfter <- sender.debugPortfoliosFor(firstAddress, considerUnspent = true)
    } yield {
      val expectedBalance = portfolioBefore.balance - 10.waves // withdraw + fee
      assert(portfolioAfter.balance == expectedBalance)
    }

    Await.result(f, 1.minute)
  }

  test("getting a balance without pessimistic transactions from UTX pool - not changed after UTX") {
    val f = for {
      portfolioBefore <- sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
      utxSizeBefore <- sender.utxSize

      _ <- sender.payment(firstAddress, secondAddress, 5.waves, fee = 5.waves)
      _ <- sender.waitForUtxIncreased(utxSizeBefore)

      portfolioAfter <- sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    } yield {
      assert(portfolioAfter.balance == portfolioBefore.balance)
    }

    Await.result(f, 1.minute)
  }
}
