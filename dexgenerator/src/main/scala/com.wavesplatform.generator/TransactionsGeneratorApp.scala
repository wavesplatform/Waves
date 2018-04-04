package com.wavesplatform.generator

import java.util.concurrent.Executors

import cats.implicits.showInterpolator
import com.typesafe.config.ConfigFactory
import com.wavesplatform.generator.cli.ScoptImplicits
import com.wavesplatform.generator.config.FicusImplicits
import com.wavesplatform.generator.utils.{ApiRequests, GenOrderType}
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.network.client.NetworkSender
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.{EnumerationReader, NameMapper}
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.Dsl.{config => clientConfig, _}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsNumber, JsObject, Json}
import scopt.OptionParser
import scorex.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.api.http.assets.{SignedIssueRequest, SignedMassTransferRequest}
import scorex.transaction.AssetId
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.MassTransferTransaction.ParsedTransfer
import scorex.transaction.assets.{IssueTransaction, MassTransferTransaction}
import scorex.utils.LoggerFacade
import settings.GeneratorSettings

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

object TransactionsGeneratorApp extends App with ScoptImplicits with FicusImplicits with EnumerationReader {

  implicit val readConfigInHyphen: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase // IDEA bug

  val log = LoggerFacade(LoggerFactory.getLogger("generator"))
  val client: AsyncHttpClient = asyncHttpClient(clientConfig().setKeepAlive(false).setNettyTimer(GlobalTimer.instance))
  val api: ApiRequests = new ApiRequests(client)

  val parser = new OptionParser[GeneratorSettings]("generator") {
    head("TransactionsGenerator - Waves load testing transactions generator")
    opt[Int]('i', "iterations").valueName("<iterations>").text("number of iterations").action { (v, c) =>
      c.copy(worker = c.worker.copy(iterations = v))
    }
    opt[FiniteDuration]('d', "delay").valueName("<delay>").text("delay between iterations").action { (v, c) =>
      c.copy(worker = c.worker.copy(delay = v))
    }
    opt[Boolean]('r', "auto-reconnect").valueName("<true|false>").text("reconnect on errors").action { (v, c) =>
      c.copy(worker = c.worker.copy(autoReconnect = v))
    }
    help("help").text("display this help message")

    cmd("dex")
      .action { (_, c) =>
        c.copy(mode = Mode.DEX)
      }
      .text("Run orders between pre-defined accounts")
      .children(
        opt[Int]("orders").abbr("t").optional().text("number of orders").action { (x, c) =>
          c.copy(dex = c.dex.copy(orders = x))
        }
      )
  }

  val defaultConfig = ConfigFactory.load().as[GeneratorSettings]("generator")

  def issueAssets(endpoint: String, richAddressSeed: String, n: Int): Seq[AssetId] = {
    val node = api.to(endpoint)

    val assetsTx: Seq[IssueTransaction] = (1 to n).map(
      _ =>
        IssueTransaction
          .create(
            PrivateKeyAccount.fromSeed(richAddressSeed).right.get,
            name = s"asset$n".getBytes(),
            description = "asset description".getBytes(),
            quantity = 99999999999L,
            decimals = 8,
            reissuable = false,
            fee = 100000000,
            timestamp = System.currentTimeMillis()
          )
          .right
          .get)

    val tradingAssets: Seq[AssetId] = assetsTx.map(tx => tx.id())
    val signedIssueRequests: Seq[SignedIssueRequest] = assetsTx.map(tx => api.createSignedIssueRequest(tx))

    val issued: Seq[Future[Transaction]] = signedIssueRequests
      .map { txReq =>
        node.signedIssue(txReq).flatMap { tx =>
          node.waitForTransaction(tx.id)
        }
      }

    val allIssued: Future[Seq[Transaction]] = Future.sequence(issued)
    Await.result(allIssued, 30.seconds)
    tradingAssets
  }

  def parsedTransfersList(endpoint: String, asset: Option[String], pk: PrivateKeyAccount, accounts: Seq[PrivateKeyAccount]): List[ParsedTransfer] = {
    val client: AsyncHttpClient = asyncHttpClient(clientConfig().setKeepAlive(false).setNettyTimer(GlobalTimer.instance))
    val api: ApiRequests = new ApiRequests(client)

    val node = api.to(endpoint)
    var richAccountBalance = Await.result(
      asset match {
        case None => node.balance(PublicKeyAccount(pk.publicKey).address).map(_.balance)
        case Some(assetId) =>
          node.assetBalance(PublicKeyAccount(pk.publicKey).address, assetId).map(_.balance)
      },
      30.seconds
    )

    val transferAmount: Long = (richAccountBalance / accounts.size) / 1000
    val assetsTransfers =
      accounts.map { accountPk =>
        ParsedTransfer(AddressOrAlias.fromString(PublicKeyAccount(accountPk.publicKey).address).right.get, transferAmount)
      }
    assetsTransfers.toList
  }

  def massTransferWaves(endpoint: String, richAccountSeed: String, accounts: Seq[PrivateKeyAccount]) = {
    val node = api.to(endpoint)
    val pk = PrivateKeyAccount.fromSeed(richAccountSeed).right.get

    val wavesTransfers = parsedTransfersList(endpoint, None, pk, accounts)

    val massTransferTx: MassTransferTransaction =
      MassTransferTransaction
        .selfSigned(
          MassTransferTransaction.Version,
          None,
          PrivateKeyAccount.fromSeed(richAccountSeed).right.get,
          wavesTransfers,
          System.currentTimeMillis(),
          100000 + 50000 * wavesTransfers.size,
          Array.emptyByteArray
        )
        .right
        .get

    implicit val w =
      Json.writes[SignedMassTransferRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(TransactionType.MassTransferTransaction.id)))

    val signedMassTransfer: SignedMassTransferRequest = api.createSignedMassTransferRequest(massTransferTx)

    val transferred =
      node.broadcastRequest(signedMassTransfer).flatMap { tx =>
        node.waitForTransaction(tx.id)
      }
    Await.result(transferred, 30.seconds)
  }

  def massTransferAssets(endpoint: String, richAccountSeed: String, accounts: Seq[PrivateKeyAccount], tradingAssets: Seq[AssetId]) = {

    val node = api.to(endpoint)
    val pk = PrivateKeyAccount.fromSeed(richAccountSeed).right.get
    val assetsTransfers = parsedTransfersList(endpoint, Some(tradingAssets.seq.head.base58), pk, accounts)

    val massTransferTxSeq: Seq[MassTransferTransaction] = tradingAssets.map {
      assetId =>
        MassTransferTransaction
          .selfSigned(
            MassTransferTransaction.Version,
            Some(assetId),
            PrivateKeyAccount.fromSeed(richAccountSeed).right.get,
            assetsTransfers,
            System.currentTimeMillis(),
            100000 + 50000 * assetsTransfers.size,
            Array.emptyByteArray
          )
          .right
          .get
    }

    implicit val w =
      Json.writes[SignedMassTransferRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(TransactionType.MassTransferTransaction.id)))

    val signedMassTransfer: Seq[SignedMassTransferRequest] = massTransferTxSeq.map(tx => api.createSignedMassTransferRequest(tx))

    val transferred: Seq[Future[Transaction]] = signedMassTransfer
      .map { txReq =>
        node.broadcastRequest(txReq).flatMap { tx =>
          node.waitForTransaction(tx.id)
        }
      }

    val allIssued: Future[Seq[Transaction]] = Future.sequence(transferred)
    Await.result(allIssued, 30.seconds)

  }

  parser.parse(args, defaultConfig) match {
    case None => parser.failure("Failed to parse command line parameters")
    case Some(finalConfig) =>
      log.info(show"The final configuration: \n$finalConfig")

      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = finalConfig.addressScheme.toByte
      }

      val ordersDistr: Map[GenOrderType.Value, Int] = finalConfig.dex.probabilities.map {
        case (k, v) =>
          (k, Math.round(v * finalConfig.dex.orders).toInt)
      }

      val threadPool = Executors.newFixedThreadPool(Math.max(1, finalConfig.sendTo.size))
      implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(threadPool)

      val sender = new NetworkSender(finalConfig.addressScheme, "generator", nonce = Random.nextLong())
      sys.addShutdownHook(sender.close())

      val endpoint = finalConfig.sendTo.head.getHostString

      val test: GeneratorSettings = finalConfig

      val defaultPrice = 1000

      val tradingAssets = issueAssets(endpoint, finalConfig.richAccounts.head, finalConfig.assetPairsNum)

      massTransferAssets(endpoint, finalConfig.richAccounts.head, finalConfig.validAccounts, tradingAssets.take(finalConfig.assetPairsNum - 2))
      massTransferAssets(endpoint, finalConfig.richAccounts.head, finalConfig.fakeAccounts, tradingAssets.drop(finalConfig.assetPairsNum - 2))
      massTransferWaves(endpoint, finalConfig.richAccounts.head, finalConfig.validAccounts ++ finalConfig.fakeAccounts)


      val workers =
        ordersDistr.map(p => new Worker(finalConfig.worker, finalConfig, finalConfig.matcherConfig, tradingAssets, p._1, p._2, client))

      def close(status: Int): Unit = {
        sender.close()
        threadPool.shutdown()
        System.exit(status)
      }

      Future
        .sequence(workers.map(_.run()))
        .onComplete {
          case Success(_) =>
            log.info("Done")
            close(0)

          case Failure(e) =>
            log.error("Failed", e)
            close(1)
        }
  }

}
