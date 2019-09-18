package com.wavesplatform.generator

import java.net.{InetSocketAddress, URL}

import cats.syntax.apply._
import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import monix.eval.Task
import monix.execution.Scheduler
import org.asynchttpclient.AsyncHttpClient
import play.api.libs.json.Json

import scala.compat.java8.FutureConverters
import scala.concurrent.{ExecutionContext, Future}

class NewWorker(
    settings: Settings,
    transactionSource: Iterator[Transaction],
    networkSender: NetworkSender,
    node: InetSocketAddress,
    nodeRestAddress: URL,
    canContinue: () => Boolean,
    initial: Seq[Transaction],
    richAccountAddresses: Seq[String]
)(implicit httpClient: AsyncHttpClient, ec: ExecutionContext)
    extends ScorexLogging {

  def run(): Future[Unit] =
    task.runAsyncLogErr(Scheduler(ec))

  private[this] val task =
    for {
      channel <- getChannel
      channel <- withReconnect(writeInitial(channel))
      channel <- withReconnect(pullAndWrite(channel))
      _       <- closeChannel(channel)
    } yield ()

  private[this] val nodeUTXTransactionsToSendCount: Task[Int] = Task.defer {
    import org.asynchttpclient.Dsl._
    val request = get(s"$nodeRestAddress/transactions/unconfirmed/size").build()
    Task
      .fromFuture(FutureConverters.toScala(httpClient.executeRequest(request).toCompletableFuture))
      .map(r => math.max(settings.utxLimit - (Json.parse(r.getResponseBody) \ "size").as[Int], 0))
  }

  private[this] val balanceOfRichAccount: Task[Map[String, Long]] =
    Task
      .defer {
        import org.asynchttpclient.Dsl._
        val results = richAccountAddresses.map { address =>
          val request = get(s"$nodeRestAddress/addresses/balance/$address").build()
          Task
            .fromFuture(FutureConverters.toScala(httpClient.executeRequest(request).toCompletableFuture))
            .map(r => address -> (Json.parse(r.getResponseBody) \ "balance").as[Long])
        }
        Task.gather(results).map(_.toMap)
      }
      .onErrorFallbackTo(Task.now(Map()))

  private[this] val retrieveBalances: Task[Unit] =
    if (!canContinue())
      Task.unit
    else
      for {
        balances <- balanceOfRichAccount
        _        <- if (balances.nonEmpty) logInfo(s"Balances: ${balances.mkString("(", ", ", ")")}") else Task.unit
      } yield ()

  private[this] def writeInitial(channel: Channel): Task[Channel] =
    if (!canContinue())
      Task.now(channel)
    else
      for {
        validChannel <- validateChannel(channel)
        _            <- logInfo(s"Sending ${initial.size} initial transactions to $validChannel")
        _            <- Task.deferFuture(networkSender.send(validChannel, initial: _*))
        _            <- sleep
      } yield validChannel

  private[this] def pullAndWrite(channel: Channel, cnt: Int = 0): Task[Channel] =
    if (!canContinue())
      Task.now(channel)
    else
      for {
        _             <- if (cnt % 10 == 0) retrieveBalances.executeAsync else Task.unit
        validChannel  <- validateChannel(channel)
        txToSendCount <- nodeUTXTransactionsToSendCount
        _             <- logInfo(s"Sending $txToSendCount transactions to $validChannel")
        _             <- Task.deferFuture(networkSender.send(validChannel, transactionSource.take(txToSendCount).toStream: _*))
        _             <- sleep
        r             <- Task.defer(pullAndWrite(validChannel, (cnt + 1) % 10))
      } yield r

  private[this] def withReconnect[A](baseTask: Task[A]): Task[A] =
    baseTask.onErrorHandleWith {
      case error if settings.autoReconnect && canContinue() =>
        logError(s"[$node] An error during sending transactions, reconnect", error) *>
          Task.sleep(settings.reconnectDelay) *>
          Task.defer(withReconnect(baseTask))
      case error =>
        logError("Stopping because autoReconnect is disabled", error) *>
          Task.raiseError(error)
    }

  private[this] def getChannel: Task[Channel]                        = Task.deferFuture(networkSender.connect(node))
  private[this] def closeChannel(channel: Channel): Task[Unit]       = Task(channel.close())
  private[this] def validateChannel(channel: Channel): Task[Channel] = if (channel.isOpen) Task.now(channel) else getChannel

  private[this] def logError(msg: => String, err: Throwable): Task[Unit] = Task(log.error(msg, err))
  private[this] def logInfo(msg: => String): Task[Unit]                  = Task(log.info(msg))

  private[this] val sleep: Task[Unit] = logInfo(s"Sleeping for ${settings.delay}") *> Task.sleep(settings.delay)
}
