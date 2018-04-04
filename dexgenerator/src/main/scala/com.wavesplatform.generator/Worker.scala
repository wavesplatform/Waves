package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import cats.data._
import cats.implicits._
import com.wavesplatform.crypto
import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.generator.utils.{ApiRequests, GenOrderType}
import com.wavesplatform.it.api.{MatcherResponse, MatcherStatusResponse, OrderbookHistory, Transaction, UnexpectedStatusCodeException}
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.matcher.model.LimitOrder.OrderStatus
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import org.asynchttpclient.{AsyncHttpClient, Response}
import play.api.libs.json._
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.{Curve25519, PrivateKey}
import scorex.transaction.{AssetId, ValidationError}
import scorex.transaction.assets.exchange.{AssetPair, Order}
import settings.{GeneratorSettings, MatcherNodeSettings}

import scala.collection.immutable
import scala.concurrent.duration.{Duration, FiniteDuration, _}
import scala.concurrent._
import scala.util.{Failure, Success, Try}
import Worker._
import org.slf4j.LoggerFactory
import scorex.api.http.assets.{SignedMassTransferRequest, SignedTransferRequest}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.TransferTransaction
import scorex.utils.LoggerFacade

class Worker(workerSettings: Settings,
             generatorSettings: GeneratorSettings,
             matcherSettings: MatcherNodeSettings.Settings,
             tradingAssets: Seq[AssetId],
             orderType: GenOrderType.Value,
             ordersCount: Int,
             client: AsyncHttpClient)(implicit ec: ExecutionContext)
  extends ApiRequests(client) {

  log.info("started worker " + orderType)

  private type Result[T] = EitherT[Future, (Int, Throwable), T]

  def run(): Future[Unit] =
    placeOrders(1).leftMap { _ =>
      ()
    }.merge

  val endpoint = generatorSettings.sendTo.head.getHostString

  private val matcherPublicKey = PublicKeyAccount.fromBase58String(matcherSettings.matcherKey).right.get
  private val validAccounts = generatorSettings.validAccounts
  private val invalidAccounts = generatorSettings.invalidAccounts
  private val fakeAccounts = generatorSettings.fakeAccounts

  private val fee = 0.003.waves

  private def ts = System.currentTimeMillis()

  private def r = ThreadLocalRandom.current

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  def buyOrder(price: Long, amount: Long, buyer: PrivateKeyAccount, pair: AssetPair): (Order, Future[MatcherResponse]) = {
    val order = Order.buy(buyer, matcherPublicKey, pair, price, amount, ts, ts + 1.day.toMillis, fee)
    log.info("buy " + order)
    val response = to(matcherSettings.endpoint).placeOrder(order)
    log.info(order.idStr())
    (order, response)
  }

  def sellOrder(price: Long, amount: Long, seller: PrivateKeyAccount, pair: AssetPair): (Order, Future[MatcherResponse]) = {
    val order = Order.sell(seller, matcherPublicKey, pair, price, amount, ts, ts + 1.day.toMillis, fee)
    log.info("sell " + order)
    val response = to(matcherSettings.endpoint).placeOrder(order)
    log.info(order.idStr())
    (order, response)
  }

  def cancelOrder(pk: PrivateKeyAccount, pair: AssetPair, orderId: String): Future[MatcherStatusResponse] = {
    val request = CancelOrderRequest(PublicKeyAccount(pk.publicKey), Base58.decode(orderId).get, Array.emptyByteArray)
    val sig = crypto.sign(pk, request.toSign)
    val signedRequest = request.copy(signature = sig)
    to(matcherSettings.endpoint).cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest)
  }

  def isActive(status: String): Boolean = {
    status match {
      case "PartiallyFilled" =>
        true
      case "Accepted" =>
        true
      case _ =>
        false
    }
  }

  def orderHistory(account: PrivateKeyAccount): Seq[OrderbookHistory] = {
    to(matcherSettings.endpoint).orderHistory(account)
  }

  def cancelAllOrders(fakeAccounts: Seq[PrivateKeyAccount]): Future[Seq[MatcherStatusResponse]] = {
    val orderHistoriesPerAccount: Seq[(PrivateKeyAccount, Seq[OrderbookHistory])] =
      fakeAccounts.map(account => account -> orderHistory(account).filter(o => isActive(o.status)))
    log.info(s"$orderHistoriesPerAccount")
    val cancelOrders: Seq[Future[MatcherStatusResponse]] =
      orderHistoriesPerAccount.flatMap(account => account._2.map(o => cancelOrder(account._1, o.assetPair, o.id)))
    Future.sequence(cancelOrders).andThen {
      case x => log.info(s"ended with $x")
    }
  }

  def transfer(sender: PrivateKeyAccount, assetId: Option[AssetId], recipient: PrivateKeyAccount, halfBalance: Boolean): Future[Transaction] = {

    log.info("waves balance: " + to(endpoint).balance(PublicKeyAccount(sender.publicKey).address, None))
    val balance = to(endpoint).balance(PublicKeyAccount(sender.publicKey).address, assetId)
    log.info("asset balance: " + balance)
    val amount =
      assetId match {
        case None =>
          if (halfBalance)
            (balance / 2) - 0.001.waves
          else balance - 0.0001.waves

        case _ =>
          if (halfBalance)
            balance / 2
          else balance

      }

    implicit val w =
      Json.writes[SignedTransferRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(TransactionType.TransferTransaction.id)))

    val txRequest1 = TransferTransaction
      .create(assetId,
        sender,
        AddressOrAlias.fromString(PublicKeyAccount(recipient.publicKey).address).right.get,
        amount,
        ts,
        None,
        fee,
        Array.emptyByteArray)

    if (txRequest1.isLeft) log.error(s"$txRequest1")
    val txRequest = txRequest1.right.get
    val signedTx = createSignedTransferRequest(txRequest)
    to(endpoint).broadcastRequest(signedTx).flatMap { tx =>
      to(endpoint).waitForTransaction(tx.id)
    }
  }

  private def placeOrders(startStep: Int): Result[Unit] = {
    val defaultAmount = 10000
    val defaultPrice = 10000

    def loop(step: Int): Result[Unit] = {

      def trySend = Future.sequence {
        val result = (1 to ordersCount).map(_ =>
          orderType match {
            case GenOrderType.ActiveBuy =>
              log.info("always active buy")
              val buyer = randomFrom(validAccounts).get
              to(matcherSettings.endpoint).orderHistory(buyer)
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              buyOrder(defaultPrice / 100, defaultAmount, buyer, pair)._2
            case GenOrderType.ActiveSell =>
              log.info("always active sell")
              val seller = randomFrom(validAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              sellOrder(defaultPrice * 10, defaultAmount, seller, pair)._2
            case GenOrderType.Buy =>
              log.info("buy for filling")
              val buyer = randomFrom(validAccounts).get
              to(matcherSettings.endpoint).orderHistory(buyer)
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              buyOrder(defaultPrice, defaultAmount, buyer, pair)._2
            case GenOrderType.Sell =>
              log.info("sell for filling")
              val seller = randomFrom(validAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              sellOrder(defaultPrice, defaultAmount, seller, pair)._2
            case GenOrderType.Cancel =>
              log.info("cancel order")
              val buyer = randomFrom(validAccounts).get
              to(matcherSettings.endpoint).orderHistory(buyer)
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              sellOrder(defaultPrice * 15, defaultAmount, buyer, pair)._2.flatMap { orderInfo =>
                to(matcherSettings.endpoint).orderHistory(buyer)
                cancelOrder(buyer, pair, orderInfo.message.id)
              }
            case GenOrderType.InvalidAmount =>
              log.info("invalid amount")
              val invalidBuyer = randomFrom(invalidAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              buyOrder(defaultPrice, defaultAmount, invalidBuyer, pair)._2
                .transformWith {
                  case Failure(e: UnexpectedStatusCodeException) => e.statusCode match {
                    case 400 => Future.successful(())
                    case code => Future.failed(e)
                  }
                  case Success(x) => Future.failed(new IllegalStateException(s"Order should not be placed: ${x}"))
                }
            case GenOrderType.FakeSell =>
              log.info("fake sell account")
              cancelAllOrders(fakeAccounts).flatMap { _ =>
                val seller: PrivateKeyAccount = fakeAccounts.head
                val buyer: PrivateKeyAccount = fakeAccounts(1)
                val pair = AssetPair(randomFrom(tradingAssets.drop(tradingAssets.size - 2)), None)
                sellOrder(defaultPrice, defaultAmount, seller, pair)._2
                val hist = fakeAccounts.map(account => account -> to(matcherSettings.endpoint).orderHistory(account).filter(o => isActive(o.status)))
                log.info(s"$hist")
                transfer(seller, pair.amountAsset, buyer, halfBalance = false).flatMap { _ =>
                  to(matcherSettings.endpoint).orderHistoryFuture(seller).flatMap { _ =>
                    buyOrder(defaultPrice, defaultAmount, buyer, pair)._2
                    to(matcherSettings.endpoint).orderHistoryFuture(seller).flatMap { _ =>
                      transfer(buyer, pair.amountAsset, seller, halfBalance = true)
                    }
                  }
                }
              }
            case GenOrderType.FakeBuy =>
              log.info("fake buy account")
              cancelAllOrders(fakeAccounts).flatMap { _ =>
                val seller: PrivateKeyAccount = fakeAccounts(2)
                val buyer: PrivateKeyAccount = fakeAccounts(3)
                val pair = AssetPair(randomFrom(tradingAssets.drop(tradingAssets.size - 2)), None)
                buyOrder(defaultPrice, defaultAmount, buyer, pair)._2
                transfer(buyer, pair.amountAsset, seller, halfBalance = false).flatMap { _ =>
                  to(matcherSettings.endpoint).orderHistoryFuture(buyer).flatMap { _ =>
                    sellOrder(defaultPrice, defaultAmount, seller, pair)._2
                    to(matcherSettings.endpoint).orderHistoryFuture(buyer).flatMap { _ =>
                      transfer(seller, pair.amountAsset, buyer, halfBalance = true)
                    }
                  }
                }
              }
          })
        result
      }

      def xxx: EitherT[Future, Nothing, immutable.IndexedSeq[Any]] = EitherT.liftF(trySend)

      def next: Result[Unit] = {
        log.info(s"${ordersCount} orders had been sent")
        if (step < workerSettings.iterations) {
          log.info(s" Sleeping for ${workerSettings.delay}")

          EitherT
            .liftF(Future {
              blocking {
                Thread.sleep(workerSettings.delay.toMillis)
              }
            })
            .flatMap { _ =>
              loop(step + 1)
            }
        } else {
          log.info(s"Done")
          EitherT.right(Future.successful(Right(())))
        }
      }

      xxx.flatMap(_ => next)
    }

    loop(startStep)
  }


  override protected def log: LoggerFacade = LoggerFacade(LoggerFactory.getLogger(s"${this.getClass.getSimpleName}$hashCode()"))

  override def toString: String = s"${super.toString}($hashCode())"
}

object Worker {

  case class Settings(autoReconnect: Boolean, iterations: Int, delay: FiniteDuration, reconnectDelay: FiniteDuration)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._

      s"""number of iterations: $iterations
         |delay between iterations: $delay
         |auto reconnect: ${if (autoReconnect) "enabled" else "disabled"}
         |reconnect delay: $reconnectDelay""".stripMargin

    }
  }

}

object AssetPairCreator {
  val WavesName = "WAVES"

  private def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case `WavesName` => Success(None)
    case other => ByteStr.decodeBase58(other).map(Option(_))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)
}
