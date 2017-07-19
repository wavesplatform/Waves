package com.wavesplatform.it

import com.wavesplatform.it.TransferSending.Req
import com.wavesplatform.it.api.NodeApi.Transaction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

object TransferSending {
  case class Req(source: String, targetAddress: String, amount: Long, fee: Long)
}

trait TransferSending {
  def nodes: Seq[Node]

  def generateRequests(n: Int, balances: Map[String, Long]): Seq[Req] = {
    val addresses = nodes.map(_.address)
    val sourceAndDest = (1 to n).map { _ =>
      val Seq(src, dest) = Random.shuffle(addresses).take(2)
      (src, dest)
    }
    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (reqs, (src, dest)) =>
        val transferAmount = (Random.nextDouble() * 1e-8 * balances(src)).toLong
        reqs :+ Req(src, dest, transferAmount, 100000)
    }

    requests
  }

  def balanceForNode(n: Node): Future[(String, Long)] = n.balance(n.address).map(b => b.address -> b.balance)

  def makeTransfer(r: Req): Future[Transaction] = {
    val addressToNode = nodes.map(n => n.address -> n).toMap
    addressToNode(r.source).transfer(r.source, r.targetAddress, r.amount, r.fee)
  }

  def processRequests(reqs: Seq[Req]): Future[Unit] = if (reqs.isEmpty) {
    Future.successful(())
  } else {
    makeTransfer(reqs.head).flatMap(_ => processRequests(reqs.tail))
  }

}
