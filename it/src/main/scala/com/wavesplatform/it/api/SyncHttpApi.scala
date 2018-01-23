package com.wavesplatform.it.api

import com.wavesplatform.it.Node
import org.scalatest.{Assertions, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object SyncHttpApi {


  implicit class NodeExtSync(n: Node) extends Assertions with Matchers {

    import com.wavesplatform.it.api.AsyncHttpApi.{NodeAsyncHttpApi => async}

    private val RequestAwaitTime = 15.seconds

    def accountBalances(acc: String): (Long, Long) =
      Await.result(async(n).accountBalances(acc), RequestAwaitTime)

    def assertBalances(acc: String, balance: Long, effectiveBalance: Long): Unit =
      Await.result(async(n).assertBalances(acc, balance, effectiveBalance), RequestAwaitTime)

    def assertAssetBalance(acc: String, assetIdString: String, balance: Long): Unit =
      Await.result(async(n).assertAssetBalance(acc, assetIdString, balance), RequestAwaitTime)

    def issue(sourceAddress: String, name: String, description: String, quantity: Long, decimals: Byte, reissuable: Boolean, fee: Long): Transaction =
      Await.result(async(n).issue(sourceAddress, name, description, quantity, decimals, reissuable, fee), RequestAwaitTime)

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long): Transaction =
      Await.result(async(n).burn(sourceAddress, assetId, quantity, fee), RequestAwaitTime)
  }

  implicit class NodesExtSync(nodes: Seq[Node]) {

    import com.wavesplatform.it.api.AsyncHttpApi.{NodesAsyncHttpApi => async}


    private val TxInBlockchainAwaitTime = 3 * nodes.head.blockDelay
    private val ConditionAwaitTime = 5.minutes

    def waitForHeightAraiseAndTxPresent(transactionId: String): Unit =
      Await.result(async(nodes).waitForHeightAraiseAndTxPresent(transactionId), TxInBlockchainAwaitTime)

    def waitForHeightAraise(): Unit =
      Await.result(async(nodes).waitForHeightAraise(), TxInBlockchainAwaitTime)

    def waitForSameBlocksAt(retryInterval: FiniteDuration, height: Int): Boolean =
      Await.result(async(nodes).waitForSameBlocksAt(retryInterval, height), ConditionAwaitTime)

    def waitFor[A](desc: String)(retryInterval: FiniteDuration)(request: Node => A, cond: Iterable[A] => Boolean): Boolean =
      Await.result(async(nodes).waitFor(desc)(retryInterval)((n: Node) => Future(request(n))(scala.concurrent.ExecutionContext.Implicits.global), cond), ConditionAwaitTime)
  }

}
