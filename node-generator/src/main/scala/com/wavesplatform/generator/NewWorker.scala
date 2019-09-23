package com.wavesplatform.generator

import java.net.{InetSocketAddress, URL}

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

class NewWorker(settings: Settings,
                transactionSource: Iterator[Transaction],
                networkSender: NetworkSender,
                node: InetSocketAddress,
                nodeRestAddress: URL,
                canContinue: () => Boolean,
                initial: Seq[Transaction])(implicit httpClient: AsyncHttpClient, ec: ExecutionContext)
    extends ScorexLogging {

  def run(): Future[Unit] =
    pullAndWriteTask().map(_ => ()).runAsyncLogErr(Scheduler(ec))

  private[this] def pullAndWriteTask(channel: Option[Channel] = None): Task[Option[Channel]] =
    if (!canContinue())
      Task.now(None)
    else {
      val nodeUTXTransactionsCount: Task[Int] = Task.defer {
        import org.asynchttpclient.Dsl._
        val request = get(s"$nodeRestAddress/transactions/unconfirmed/size").build()
        Task
          .fromFuture(FutureConverters.toScala(httpClient.executeRequest(request).toCompletableFuture))
          .map(r => (Json.parse(r.getResponseBody) \ "size").as[Int])
      }

      def writeTransactions(channel: Channel, txs: Seq[Transaction]): Task[Unit] =
        for {
          _ <- Task.fromFuture(networkSender.send(channel, initial: _*))
          _ <- Task.fromFuture(networkSender.send(channel, txs: _*))
        } yield ()

      val baseTask = for {
        validChannel <- Task.defer(if (channel.exists(_.isOpen)) Task.now(channel.get) else Task.fromFuture(networkSender.connect(node)))
        txCount <- nodeUTXTransactionsCount
        txToSendCount = settings.utxLimit - txCount
        _ = log.info(s"Sending $txToSendCount to $validChannel")
        _       <- writeTransactions(validChannel, transactionSource.take(txToSendCount).toStream)
      } yield Option(validChannel)

      val withReconnect = baseTask.onErrorRecoverWith {
        case error =>
          channel.foreach(_.close())

          if (settings.autoReconnect) {
            log.error(s"[$node] An error during sending transactions, reconnect", error)
            for {
              _       <- Task.sleep(settings.reconnectDelay)
              channel <- pullAndWriteTask()
            } yield channel
          } else {
            log.error("Stopping because autoReconnect is disabled", error)
            Task.raiseError(error)
          }
      }

      for {
        channel <- withReconnect
        _ <- Task(log.info(s"Sleeping for ${settings.delay}"))
        _ <- Task.sleep(settings.delay)
        newChannel <- pullAndWriteTask(channel)
      } yield newChannel
    }
}
