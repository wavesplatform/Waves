package com.wavesplatform.it

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.{sequence, traverse}
import scala.concurrent.duration._
import com.wavesplatform.it.util._

class BlockHeadersSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

  test("") {
    val f = for {
      transferId <- sender.transfer(firstAddress, secondAddress, 5.waves, fee = 5.waves).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(transferId, 1)


    } yield succeed

    Await.result(f, 1.minute)
  }

}
