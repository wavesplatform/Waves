package com.wavesplatform.it

import com.wavesplatform.it.TransferSending.Req
import com.wavesplatform.it.api.NodeApi.Transaction
import scorex.account.{Address, AddressScheme}
import scorex.utils.ScorexLogging

import scala.concurrent.Future
import scala.util.Random

object TransferSending {
  case class Req(source: String, targetAddress: String, amount: Long, fee: Long)
}

trait TransferSending extends ScorexLogging {

  import scala.concurrent.ExecutionContext.Implicits.global

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'I'.toByte
  }

  def nodes: Seq[Node]
  private lazy val addressToNode = nodes.map(n => n.address -> n).toMap

  def generateTransfersBetweenAccounts(n: Int, balances: Map[String, Long]): Seq[Req] = {
    val fee = 100000
    val addresses = nodes.map(_.address)
    val sourceAndDest = (1 to n).map { _ =>
      val Seq(src, dest) = Random.shuffle(addresses).take(2)
      (src, dest)
    }
    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (src, dest)) =>
        val a = Random.nextDouble()
        val b = balances(src)
        val transferAmount = (1e-8 + a * 1e-9 * b).toLong
        if (transferAmount < 0) log.warn(s"Negative amount: (1e-8 + $a * 1e-8 * $b) = $transferAmount")
        rs :+ Req(src, dest, Math.max(transferAmount, 1L), fee)
    }

    requests
  }

  def generateTransfersToRandomAddresses(n: Int, balances: Map[String, Long]): Seq[Req] = {
    val fee = 100000
    val seedSize = 32
    val addresses = nodes.map(_.address)

    val sourceAndDest = (1 to n).map { _ =>
      val src = Random.shuffle(addresses).head
      val pk = Array.fill[Byte](seedSize)(Random.nextInt(Byte.MaxValue).toByte)
      val dst = Address.fromPublicKey(pk).address

      (src, dst)
    }
    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (src, dst)) =>
        rs :+ Req(src, dst, fee, fee)
    }

    requests
  }

  def balanceForNode(n: Node): Future[(String, Long)] = n.balance(n.address).map(b => b.address -> b.balance)

  def makeTransfer(r: Req): Future[Transaction] = {
    val node = addressToNode(r.source)
    log.trace(s"Sending request $r to ${node.settings.networkSettings.nodeName}")
    node.transfer(r.source, r.targetAddress, r.amount, r.fee)
  }

  def processRequests(requests: Seq[Req]): Future[Unit] = if (requests.isEmpty) {
    Future.successful(())
  } else {
    makeTransfer(requests.head).flatMap(_ => processRequests(requests.tail))
  }

}
