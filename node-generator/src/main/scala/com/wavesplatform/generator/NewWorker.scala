package com.wavesplatform.generator

import java.net.{InetSocketAddress, URL}

import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.network.RawBytes
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import org.asynchttpclient.AsyncHttpClient

import scala.compat.java8.FutureConverters
import scala.concurrent.{ExecutionContext, Future}

class NewWorker(settings: Settings,
                transactionSource: Iterator[Transaction],
                networkSender: NetworkSender,
                node: InetSocketAddress,
                nodeRestAddress: URL,
                canContinue: () => Boolean)(implicit httpClient: AsyncHttpClient, ec: ExecutionContext)
    extends ScorexLogging {

  def run(): Future[Unit] =
    pullAndWriteTask.runAsyncLogErr(Scheduler(ec))

  private[this] def utxSpace: Task[Int] = Task.defer {
    import org.asynchttpclient.Dsl._
    val request = get(s"$nodeRestAddress/transactions/unconfirmed/size").build()
    Task
      .fromFuture(FutureConverters.toScala(httpClient.executeRequest(request).toCompletableFuture))
      .map(_.getResponseBody.toInt)
  }

  private[this] def writeTransactions(txs: Iterator[Transaction]): Task[Unit] = Task.fromFuture {
    networkSender.connect(node).flatMap(networkSender.send(_, txs.map(RawBytes.from).toSeq: _*))
  }

  private[this] def pullAndWriteTask: Task[Unit] =
    if (!canContinue()) Task(())
    else {
      val baseTask = for {
        txCount <- utxSpace
        _       <- writeTransactions(transactionSource.take(txCount))
      } yield ()

      val withReconnect = baseTask.onErrorRecoverWith {
        case e =>
          if (settings.autoReconnect) {
            log.error("Stopping because autoReconnect is disabled", e)
            Task.raiseError(e)
          } else {
            log.error(s"[$node] An error during sending transations, reconnect", e)
            for {
              _ <- Task.sleep(settings.reconnectDelay)
              _ <- pullAndWriteTask
            } yield ()
          }
      }

      withReconnect.flatMap(_ => pullAndWriteTask)
    }
}
