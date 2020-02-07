package com.wavesplatform.it

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.Config
import com.wavesplatform.account._
import com.wavesplatform.api.http.requests.TransferRequest
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.TransferSending.Req
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.Suite
import play.api.libs.json.Json.toJson
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.Future
import scala.util.Random

object TransferSending {
  case class Req(senderSeed: String, targetAddress: String, amount: Long, fee: Long)
}

trait TransferSending extends ScorexLogging with IntegrationTestsScheme {
  this: Suite with Nodes =>

  import scala.concurrent.ExecutionContext.Implicits.global

  def generateTransfersFromAccount(n: Int, accountAddress: String): Seq[Req] = {
    val fee      = 100000 + 400000 // + 400000 for scripted accounts
    val seedSize = 32

    val srcSeed = NodeConfigs.Default
      .collectFirst {
        case x if x.getString("address") == accountAddress => x.getString("account-seed")
      }
      .getOrElse(throw new RuntimeException(s"Can't find address '$accountAddress' in nodes.conf"))

    val sourceAndDest = (1 to n).map { _ =>
      val destPk = Array.fill[Byte](seedSize)(Random.nextInt(Byte.MaxValue).toByte)
      Address.fromPublicKey(PublicKey(destPk)).stringRepr
    }

    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, dstAddr) =>
        rs :+ Req(srcSeed, dstAddr, fee, fee)
    }

    requests
  }

  def generateTransfersBetweenAccounts(n: Int, balances: Map[Config, Long]): Seq[Req] = {
    val fee = 100000
    val srcDest = balances.toSeq
      .map {
        case (config, _) =>
          val accountSeed = config.getString("account-seed")
          (config, KeyPair(Base58.decode(accountSeed)))
      }

    val sourceAndDest = (1 to n).map { _ =>
      val Seq((srcConfig, _), (_, destPrivateKey)) = Random.shuffle(srcDest).take(2)
      (srcConfig, destPrivateKey.stringRepr)
    }

    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (srcConfig, destAddr)) =>
        val a              = Random.nextDouble()
        val b              = balances(srcConfig)
        val transferAmount = (1e-8 + a * 1e-9 * b).toLong
        if (transferAmount < 0) log.warn(s"Negative amount: (1e-8 + $a * 1e-8 * $b) = $transferAmount")
        rs :+ Req(srcConfig.getString("account-seed"), destAddr, Math.max(transferAmount, 1L), fee)
    }

    requests
  }

  def generateTransfersToRandomAddresses(n: Int, excludeSrcAddresses: Set[String]): Seq[Req] = {
    val fee      = 100000
    val seedSize = 32

    val seeds = NodeConfigs.Default.collect {
      case config if !excludeSrcAddresses.contains(config.getString("address")) => config.getString("account-seed")
    }

    val sourceAndDest = (1 to n).map { _ =>
      val srcSeed  = Random.shuffle(seeds).head
      val destPk   = Array.fill[Byte](seedSize)(Random.nextInt(Byte.MaxValue).toByte)
      val destAddr = Address.fromPublicKey(PublicKey(destPk)).stringRepr

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
    val n     = requests.size
    val start = System.currentTimeMillis() - n
    val requestGroups = requests.zipWithIndex
      .map {
        case (x, i) =>
          createSignedTransferRequest(
            TransferTransaction
              .selfSigned(
                version = 2.toByte,
                sender = KeyPair(Base58.decode(x.senderSeed)),
                recipient = AddressOrAlias.fromString(x.targetAddress).explicitGet(),
                asset = Waves,
                amount = x.amount,
                feeAsset = Waves,
                fee = x.fee,
                attachment =
                  if (includeAttachment) Some(Attachment.Bin(Array.fill(TransferTransaction.MaxAttachmentSize)(ThreadLocalRandom.current().nextInt().toByte)))
                  else None,
                timestamp = start + i
              )
              .right
              .get
          )
      }
      .grouped(requests.size / nodes.size)
      .toSeq

    Future
      .traverse(nodes.zip(requestGroups)) {
        case (node, request) =>
          request.foldLeft(Future.successful(Seq.empty[Transaction])) {
            case (f, r) =>
              f.flatMap(ts => node.signedBroadcast(toJson(r).as[JsObject] ++ Json.obj("type" -> TransferTransaction.typeId.toInt)).map(_ +: ts))
          }
      }
      .map(_.flatten)
  }

  protected def createSignedTransferRequest(tx: TransferTransaction): TransferRequest = {
    import tx._
    TransferRequest(
      Some(2.toByte),
      None,
      Some(Base58.encode(tx.sender)),
      recipient.stringRepr,
      Some(assetId),
      amount,
      Some(feeAssetId),
      fee,
      attachment,
      Some(timestamp),
      None,
      Some(proofs)
    )
  }

}
