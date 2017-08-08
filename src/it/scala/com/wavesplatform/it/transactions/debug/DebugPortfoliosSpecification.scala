package com.wavesplatform.it.transactions.debug

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}
import scorex.account.Address

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class DebugPortfoliosSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

  test("getting a balance considering pessimistic transactions from UTX pool") {
    val firstAddr = Address.fromString(firstAddress).right.get

    val f = for {
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)
      _ <- assertBalances(secondAddress, 100.waves, 100.waves)

      portfolioBefore <- sender.debugPortfoliosFor(firstAddr)
      utxSizeBefore <- sender.utxSize

      _ <- sender.payment(firstAddress, secondAddress, 5.waves, fee = 5.waves)
      _ <- sender.waitForUtxIncreased(utxSizeBefore)

      portfolioAfter <- sender.debugPortfoliosFor(firstAddr)
    } yield {
      val expectedBalance = portfolioBefore.balance - 10.waves // withdraw + fee
      assert(portfolioAfter.balance == expectedBalance)
    }

    Await.result(f, 1.minute)
  }
}
