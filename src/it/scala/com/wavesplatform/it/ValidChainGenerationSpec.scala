package com.wavesplatform.it

import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.Random

class ValidChainGenerationSpec(allNodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers {

  import ValidChainGenerationSpec._

  "Generate 30 blocks and synchronise" in {

    val addressToNode = allNodes.map(n => n.address -> n).toMap
    val addresses = allNodes.map(_.address)
    val sourceAndDest = (1 to 1000).map { _ =>
      val Seq(src, dest) = Random.shuffle(addresses).take(2)
      (src, dest)
    }

    def generateRequests(balances: mutable.Map[String, Long]) = {
      val (_, requests) = sourceAndDest.foldLeft((balances, ArrayBuffer.empty[Req])) {
        case ((b, reqs), (src, dest)) =>
          val transferAmount = (Random.nextDouble() * 1e-3 * b(src)).toLong

          b += src -> (b(src) - transferAmount)
          b += dest -> (b(dest) + transferAmount)

          reqs += Req(src, dest, transferAmount, 100000)

          (b, reqs)
      }

      requests
    }

    def balanceForNode(n: Node) = n.balance(n.address).map(b => b.address -> b.balance)

    def makeTransfer(r: Req) = addressToNode(r.source).transfer(r.source, r.targetAddress, r.amount, r.fee)

    def processRequests(reqs: Seq[Req]): Future[Unit] = if (reqs.isEmpty) {
      Future.successful(())
    } else {
      makeTransfer(reqs.head).flatMap(_ => processRequests(reqs.tail))
    }

    val targetBlocks = result(for {
      b <- traverse(allNodes)(balanceForNode).map(mutable.AnyRefMap[String, Long](_: _*))
      _ <- processRequests(generateRequests(b))
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 40)) // wait a little longer to prevent rollbacks...
      blocks <- traverse(allNodes)(_.waitForHeight(height + 35)) // ...before requesting actual blocks
    } yield blocks, 5.minutes)

    all(targetBlocks) shouldEqual targetBlocks.head
  }
}

object ValidChainGenerationSpec {

  case class Req(source: String, targetAddress: String, amount: Long, fee: Long)

}
