package com.wavesplatform.dexgen

import java.util.concurrent.Executors

import cats.implicits.showInterpolator
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount}
import com.wavesplatform.api.http.assets.{SignedIssueV2Request, SignedMassTransferRequest}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dexgen.cli.ScoptImplicits
import com.wavesplatform.dexgen.config.FicusImplicits
import com.wavesplatform.dexgen.utils.{ApiRequests, GenOrderType}
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.utils.LoggerFacade
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.{EnumerationReader, NameMapper}
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.Dsl.{config => clientConfig, _}
import org.slf4j.LoggerFactory
import play.api.libs.json._
import scopt.OptionParser
import settings.GeneratorSettings

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

object DexGenApp extends App with ScoptImplicits with FicusImplicits with EnumerationReader {

  implicit val readConfigInHyphen: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase // IDEA bug

  val log                     = LoggerFacade(LoggerFactory.getLogger("dexgenerator"))
  val client: AsyncHttpClient = asyncHttpClient(clientConfig().setKeepAlive(false).setNettyTimer(GlobalTimer.instance))
  val api: ApiRequests        = new ApiRequests(client)

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

  implicit val signedMassTransferRequestWrites: Writes[SignedMassTransferRequest] =
    Json
      .writes[SignedMassTransferRequest]
      .transform((jsobj: JsObject) => jsobj + ("version" -> JsNumber(1)) + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt)))

  val defaultConfig = ConfigFactory.load().as[GeneratorSettings]("generator")
  AddressScheme.current = new AddressScheme { override val chainId: Byte = defaultConfig.chainId.head.toByte }

  def issueAssets(endpoint: String, richAddressSeed: String, n: Int)(implicit tag: String): Seq[Asset] = {
    val node = api.to(endpoint)

    val now = System.currentTimeMillis()
    val assetsTx: Seq[IssueTransactionV2] = (1 to n).map { i =>
      IssueTransactionV2
        .selfSigned(
          chainId = AddressScheme.current.chainId,
          name = s"asset$i".getBytes(),
          description = s"asset description - $i".getBytes(),
          quantity = 999999999999999L,
          decimals = 2,
          reissuable = false,
          fee = 100000000,
          timestamp = now + i,
          sender = PrivateKeyAccount.fromSeed(richAddressSeed).explicitGet(),
          script = None
        )
        .explicitGet()
    }

    val tradingAssets: Seq[Asset]                      = assetsTx.map(tx => IssuedAsset(tx.id()))
    val signedIssueRequests: Seq[SignedIssueV2Request] = assetsTx.map(tx => api.createSignedIssueRequest(tx))

    val issued: Seq[Future[Transaction]] = signedIssueRequests
      .map { txReq =>
        node.broadcastRequest(txReq).flatMap { tx =>
          node.waitForTransaction(tx.id)
        }
      }

    val allIssued: Future[Seq[Transaction]] = Future.sequence(issued)
    Await.result(allIssued, 120.seconds)
    tradingAssets
  }

  def parsedTransfersList(endpoint: String, assetId: Asset, transferAmount: Long, pk: PrivateKeyAccount, accounts: Seq[PrivateKeyAccount])(
      implicit tag: String): List[ParsedTransfer] = {
    val assetsTransfers = accounts.map { accountPk =>
      ParsedTransfer(AddressOrAlias.fromString(accountPk.address).right.get, transferAmount)
    }
    assetsTransfers.toList
  }

  def massTransfer(endpoint: String, richAccountSeed: String, accounts: Seq[PrivateKeyAccount], tradingAssets: Seq[Asset])(
      implicit tag: String): Future[Seq[Transaction]] = {
    val node          = api.to(endpoint)
    val richAccountPk = PrivateKeyAccount.fromSeed(richAccountSeed).right.get

    val massTransferTxSeq: Seq[Future[Transaction]] = tradingAssets.map { assetId =>
      node
        .balance(richAccountPk.address, assetId)
        .flatMap { balance =>
          val transferAmount =
            if (assetId == Waves)
              30000000000l
            else
              balance / accounts.size / 1000
          val assetsTransfers = parsedTransfersList(endpoint, assetId, transferAmount, richAccountPk, accounts)
          val tx = MassTransferTransaction
            .selfSigned(
              assetId = assetId,
              sender = richAccountPk,
              transfers = assetsTransfers,
              timestamp = System.currentTimeMillis(),
              feeAmount = 200000 + 50000 * (assetsTransfers.size + 1),
              attachment = Array.emptyByteArray
            )
            .right
            .get

          val txReq = api.createSignedMassTransferRequest(tx)
          node.broadcastRequest(txReq).flatMap(tx => node.waitForTransaction(tx.id))
        }
    }

    Future.sequence(massTransferTxSeq)
  }

  parser.parse(args, defaultConfig) match {
    case None => parser.failure("Failed to parse command line parameters")
    case Some(finalConfig) =>
      implicit val rootTag: String = "Prepare"

      log.info(show"The final configuration: \n$finalConfig")

      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = finalConfig.addressScheme.toByte
      }

      val ordersDistr: Map[GenOrderType.Value, Int] = finalConfig.dex.probabilities.map {
        case (k, v) =>
          (k, Math.round(v * finalConfig.dex.orders).toInt)
      }

      val threadPool                            = Executors.newFixedThreadPool(ordersDistr.size + 1)
      implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(threadPool)

      val sender = new NetworkSender(finalConfig.addressScheme, "generator", nonce = Random.nextLong())
      sys.addShutdownHook(sender.close())

      val endpoint = finalConfig.sendTo.head.getHostString

      val tradingAssets = issueAssets(endpoint, finalConfig.richAccounts.head, finalConfig.assetPairsNum)
      Thread.sleep(5000)

      val transfers = Seq(
        massTransfer(endpoint, finalConfig.richAccounts.head, finalConfig.validAccounts, Waves +: tradingAssets.dropRight(2)),
        massTransfer(endpoint, finalConfig.richAccounts.head, finalConfig.fakeAccounts, Waves +: tradingAssets.takeRight(2))
      )

      Await.ready(Future.sequence(transfers), 120.seconds)
      Thread.sleep(10000)

      log.info(s"Running ${ordersDistr.size} workers")
      val workers = ordersDistr.map(p => new Worker(finalConfig.worker, finalConfig, finalConfig.matcherConfig, tradingAssets, p._1, p._2, client))

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
