package com.wavesplatform.it.api

import com.wavesplatform.it.api.AsyncHttpApi.{NodeAsyncHttpApi => async, NodesAsyncHttpApi => asyncs}
import com.wavesplatform.it.api.Node.Transaction
import org.scalatest.{Assertions, Matchers}
import scala.concurrent.Await
import scala.concurrent.duration._

object SyncHttpApi {


  implicit class NodeExtSync(n: Node) extends Assertions with Matchers {

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

    private val TxInBlockchainAwaitTime = 3 * nodes.head.blockDelay

    def waitForHeightAraiseAndTxPresent(transactionId: String): Unit =
      Await.result(asyncs(nodes).waitForHeightAraiseAndTxPresent(transactionId), TxInBlockchainAwaitTime)

    def waitForHeightAraise(): Unit =
      Await.result(asyncs(nodes).waitForHeightAraise(), TxInBlockchainAwaitTime)
  }

}
