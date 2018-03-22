package com.wavesplatform.generator.workers

import java.io.IOException

import cats.data._
import cats.implicits._
import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.generator.utils.ApiRequests
import com.wavesplatform.it.api
import com.wavesplatform.it.util._
import com.wavesplatform.network.RawBytes
import com.wavesplatform.network.client.NetworkSender
import io.netty.channel.Channel
import org.asynchttpclient.{AsyncHttpClient, Response}
import scorex.account.PrivateKeyAccount
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.{AssetPair, Order}
import settings.MatcherNodeSettings

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.control.NonFatal

class OrdersWorker(settings: Settings,
                   matcher: MatcherNodeSettings,
                   val accounts: Seq[PrivateKeyAccount],
                   val tradingAssets: Seq[AssetId],
                   orderType: Int,
                   ordersCount: Int,
                   client: AsyncHttpClient)
                  (implicit ec: ExecutionContext) extends ApiRequests(client) {

  val sender: NetworkSender


  private type Result[T] = EitherT[Future, (Int, Throwable), T]

  def run(): Future[Unit] = guardedSend(1).leftMap { _ => () }.merge

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  private val matcherPublickKey = matcher.matcherKey
  private val fee = 0.003.waves

  private def ts = System.currentTimeMillis()


  private def guardedSend(startStep: Int): Result[Unit] = {

    tryConnect
      .flatMap(sendTransactions(startStep, _))
      .recoverWith {
        case (_, e) if !settings.autoReconnect =>
          log.error("Stopping because autoReconnect is disabled", e)
          EitherT.left(Future.failed(new IOException(s"Errors during sending transactions to $node", e)))

        case (lastStep, NonFatal(e)) if lastStep < settings.iterations =>
          log.error(s"[$node] An error during sending transations, reconnect", e)
          blocking {
            Thread.sleep(settings.reconnectDelay.toMillis)
          }
          guardedSend(lastStep)
      }
  }


  def buyOrder(): Future[Response] = {
    val pair = AssetPair(randomFrom(tradingAssets), None)
    val buyer = randomFrom(accounts).get
    val order = Order.buy(buyer, matcherPublickKey, pair, price, 0.0001.waves, ts, ts + 1.day.toMillis, fee)
    placeOrder(matcher.endpoint, order)
  }


  def sellOrder(): Future[Response] = {
    val pair = AssetPair(randomFrom(tradingAssets), None)
    val seller = randomFrom(accounts).get
    val order = Order.sell(seller, matcherPublickKey, pair, price, 0.0001.waves, ts, ts + 1.day.toMillis, fee)
    placeOrder(matcher.endpoint, order)
  }


  private def placeOrders(startStep: Int, ordersCount: Int): Result[Unit] = {
    def loop(step: Int): Result[Unit] = {
      log.info(s"[$node] Iteration $step")

      //add for 1..ordersCount
      def trySend: Future[Response] = {

        sellOrder()
      }
      //      def runInvalidOrder: Future[Unit] = Future.failed().recover { case x: IllegalArgumentException => () }

      def xxx: EitherT[Future, Nothing, Seq[api.MatcherResponse]] = EitherT.liftF(trySend)

      //      Future.successful(()).flatMap(_ => Future.successful(()).flatMap(response => Future.successful(cancel(request.id))))

      def next: Result[Unit] = {
        log.info(s"[$node] ${ordersCount} orders had been sent")
        if (step < settings.iterations) {
          log.info(s"[$node] Sleeping for ${settings.delay}")
          blocking {
            Thread.sleep(settings.delay.toMillis)
          }
          loop(step + 1)
        } else {
          log.info(s"[$node] Done")
          Future.successful(Right(()))
        }
      }

      trySend.flatMap(_ => next)
    }


    loop(startStep)
  }

  private def sendTransactions(startStep: Int, channel: Channel): Result[Unit] = {
    def loop(step: Int): Result[Unit] = {
      log.info(s"[$node] Iteration $step")
      val messages: Seq[RawBytes] = generator.next.map(tx => RawBytes(25.toByte, tx.bytes())).toSeq

      def trySend: Result[Unit] = EitherT {
        sender
          .send(channel, messages: _*)
          .map { _ => Right(()) }
          .recover {
            case NonFatal(e) => Left(step -> e)
          }
      }

      def next: Result[Unit] = {
        log.info(s"[$node] ${messages.size} transactions had been sent")
        if (step < settings.iterations) {
          log.info(s"[$node] Sleeping for ${settings.delay}")
          blocking {
            Thread.sleep(settings.delay.toMillis)
          }
          loop(step + 1)
        } else {
          log.info(s"[$node] Done")
          EitherT.right(Future.successful(Right(())))
        }
      }

      trySend.flatMap(_ => next)
    }

    loop(startStep)
  }

}


