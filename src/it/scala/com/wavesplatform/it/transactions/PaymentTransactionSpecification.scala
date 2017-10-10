package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class PaymentTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {
  test("waves payment changes waves balances and eff.b.") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)
      _ <- assertBalances(secondAddress, 100.waves, 100.waves)

      transferId <- sender.payment(firstAddress, secondAddress, 5.waves, fee = 5.waves)
      _ <- waitForHeightAraiseAndTxPresent(transferId, 1)
      _ <- assertBalances(firstAddress, 90.waves, 90.waves)
      _ <- assertBalances(secondAddress, 105.waves, 105.waves)
    } yield succeed

    Await.result(f, 1.minute)
  }
}
