package com.wavesplatform.it.api

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.it.Node
import org.scalatest.{Assertion, Assertions, Matchers}
import scorex.api.http.assets.SignedMassTransferRequest
import scorex.transaction.assets.MassTransferTransaction.Transfer

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Try}

object SyncHttpApi extends Assertions{

  def assertBadRequest2[R](f: => R): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, _)) => Assertions.assert(statusCode == StatusCodes.BadRequest.intValue)
    case Failure(e) => Assertions.fail(e)
    case _ => Assertions.fail(s"Expecting bad request")
  }

  implicit class NodeExtSync(n: Node) extends Assertions with Matchers {

    import com.wavesplatform.it.api.AsyncHttpApi.{NodeAsyncHttpApi => async}

    private val RequestAwaitTime = 15.seconds

    def accountBalances(acc: String): (Long, Long) =
      Await.result(async(n).accountBalances(acc), RequestAwaitTime)

    def assertBalances(acc: String, balance: Long, effectiveBalance: Long): Unit =
      Await.result(async(n).assertBalances(acc, balance, effectiveBalance), RequestAwaitTime)

    def assertAssetBalance(acc: String, assetIdString: String, balance: Long): Unit =
      Await.result(async(n).assertAssetBalance(acc, assetIdString, balance), RequestAwaitTime)

    def assetBalance(address: String, asset: String): AssetBalance =
      Await.result(async(n).assetBalance(address, asset), RequestAwaitTime)

    def assetsBalance(address: String): FullAssetsInfo =
      Await.result(async(n).assetsBalance(address), RequestAwaitTime)

    def issue(sourceAddress: String, name: String, description: String, quantity: Long, decimals: Byte, reissuable: Boolean, fee: Long): Transaction =
      Await.result(async(n).issue(sourceAddress, name, description, quantity, decimals, reissuable, fee), RequestAwaitTime)

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long): Transaction =
      Await.result(async(n).burn(sourceAddress, assetId, quantity, fee), RequestAwaitTime)

    def createAlias(targetAddress: String, alias: String, fee: Long): Transaction =
      Await.result(async(n).createAlias(targetAddress, alias, fee), RequestAwaitTime)

    def aliasByAddress(targetAddress: String): Seq[String] =
      Await.result(async(n).aliasByAddress(targetAddress), RequestAwaitTime)

    def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long, assetId: Option[String] = None): Transaction =
      Await.result(async(n).transfer(sourceAddress, recipient, amount, fee, assetId), RequestAwaitTime)

    def massTransfer(sourceAddress: String, transfers: List[Transfer], fee: Long, assetId: Option[String] = None): Transaction =
      Await.result(async(n).massTransfer(sourceAddress, transfers, fee, assetId), RequestAwaitTime)

    def lease(sourceAddress:String, recipient: String, leasingAmount: Long, leasingFee: Long): Transaction =
      Await.result(async(n).lease(sourceAddress, recipient, leasingAmount, leasingFee), RequestAwaitTime)

    def signedMassTransfer(tx: SignedMassTransferRequest): Transaction =
      Await.result(async(n).signedMassTransfer(tx), RequestAwaitTime)

    def ensureTxDoesntExist(txId: String): Unit =
      Await.result(async(n).ensureTxDoesntExist(txId), RequestAwaitTime)
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
