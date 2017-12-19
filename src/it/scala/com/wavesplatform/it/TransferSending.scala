package com.wavesplatform.it

import com.wavesplatform.it.TransferSending.Req
import com.wavesplatform.it.api.NodeApi.Transaction
import scorex.account.{Address, AddressOrAlias, AddressScheme, PrivateKeyAccount}
import scorex.api.http.assets.SignedTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.assets.TransferTransaction
import scorex.utils.ScorexLogging

import scala.concurrent.Future
import scala.util.Random

object TransferSending {
  case class Req(senderSeed: String, targetAddress: String, amount: Long, fee: Long)
}

trait TransferSending extends ScorexLogging {

  import scala.concurrent.ExecutionContext.Implicits.global

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'I'.toByte
  }

  def nodes: Seq[Node]

  def generateTransfersBetweenAccounts(n: Int, balances: Map[String, Long]): Seq[Req] = {
    val fee = 100000
    val privateKeys = nodes.map(_.accountSeed)
    val sourceAndDest = (1 to n).map { _ =>
      val Seq(srcSeed, destSeed) = Random.shuffle(privateKeys).take(2)
      (srcSeed, PrivateKeyAccount.fromSeed(destSeed).right.get.address)
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
    val seeds = nodes.map(_.accountSeed)

    val sourceAndDest = (1 to n).map { _ =>
      val src = Random.shuffle(seeds).head
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

  def balanceForNode(n: Node): Future[(String, Long)] = n.balance(n.address).map(b => n.accountSeed -> b.balance)

  /**
    * @return Last transaction
    */
  def processRequests(requests: Seq[Req]): Future[Option[Transaction]] = {
    val n = requests.size
    val start = System.currentTimeMillis() - n
    val xs = requests.zipWithIndex.map { case (x, i) =>
      createSignedTransferRequest(TransferTransaction
        .create(
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(x.senderSeed).right.get,
          recipient = AddressOrAlias.fromString(x.targetAddress).right.get,
          amount = x.amount,
          timestamp = start + i,
          feeAssetId = None,
          feeAmount = x.fee,
          attachment = Array.emptyByteArray
        )
        .right.get)
    }

    nodes.head.batchSignedTransfer(xs).map(_.lastOption)
  }

  private def createSignedTransferRequest(tx: TransferTransaction): SignedTransferRequest = {
    import tx._
    SignedTransferRequest(
      Base58.encode(tx.sender.publicKey),
      assetId.map(_.base58),
      recipient.stringRepr,
      amount,
      fee,
      feeAssetId.map(_.base58),
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      signature.base58
    )
  }

}
