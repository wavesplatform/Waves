package com.wavesplatform.it

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.Config
import com.wavesplatform.it.TransferSending.Req
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api.Transaction
import org.scalatest.Suite
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
  this: Suite with Nodes =>

  import scala.concurrent.ExecutionContext.Implicits.global

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'I'.toByte
  }

  def generateTransfersBetweenAccounts(n: Int, balances: Map[Config, Long]): Seq[Req] = {
    val fee = 100000
    val srcDest = balances
      .toSeq
      .map {
        case (config, _) =>
          val accountSeed = config.getString("account-seed")
          (config, PrivateKeyAccount.fromSeed(accountSeed).right.get)
      }

    val sourceAndDest = (1 to n).map { _ =>
      val Seq((srcConfig, _), (_, destPrivateKey)) = Random.shuffle(srcDest).take(2)
      (srcConfig, destPrivateKey.address)
    }

    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (srcConfig, destAddr)) =>
        val a = Random.nextDouble()
        val b = balances(srcConfig)
        val transferAmount = (1e-8 + a * 1e-9 * b).toLong
        if (transferAmount < 0) log.warn(s"Negative amount: (1e-8 + $a * 1e-8 * $b) = $transferAmount")
        rs :+ Req(srcConfig.getString("account-seed"), destAddr, Math.max(transferAmount, 1L), fee)
    }

    requests
  }

  def generateTransfersToRandomAddresses(n: Int, excludeSrcAddresses: Set[String]): Seq[Req] = {
    val fee = 100000
    val seedSize = 32

    val seeds = NodeConfigs.Default.collect {
      case config if !excludeSrcAddresses.contains(config.getString("address")) => config.getString("account-seed")
    }

    val sourceAndDest = (1 to n).map { _ =>
      val srcSeed = Random.shuffle(seeds).head
      val destPk = Array.fill[Byte](seedSize)(Random.nextInt(Byte.MaxValue).toByte)
      val destAddr = Address.fromPublicKey(destPk).address

      (srcSeed, destAddr)
    }
    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (srcSeed, dstAddr)) =>
        rs :+ Req(srcSeed, dstAddr, fee, fee)
    }

    requests
  }

  def balanceForNode(n: Node): Future[(String, Long)] = n.balance(n.address).map(b => n.address -> b.balance)

  def processRequests(requests: Seq[Req], includeAttachment: Boolean = false): Future[Seq[Transaction]] = {
    val n = requests.size
    val start = System.currentTimeMillis() - n
    val requestGroups = requests
      .zipWithIndex
      .map { case (x, i) =>
        createSignedTransferRequest(TransferTransaction
          .create(
            assetId = None,
            sender = PrivateKeyAccount.fromSeed(x.senderSeed).right.get,
            recipient = AddressOrAlias.fromString(x.targetAddress).right.get,
            amount = x.amount,
            timestamp = start + i,
            feeAssetId = None,
            feeAmount = x.fee,
            attachment = if (includeAttachment) {
              Array.fill(TransferTransaction.MaxAttachmentSize)(ThreadLocalRandom.current().nextInt().toByte)
            } else Array.emptyByteArray
          )
          .right.get)
      }
      .grouped(requests.size / nodes.size)
      .toSeq

    Future
      .traverse(nodes.zip(requestGroups)) { case (node, request) => node.batchSignedTransfer(request) }
      .map(_.flatten)
  }

  protected def createSignedTransferRequest(tx: TransferTransaction): SignedTransferRequest = {
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
