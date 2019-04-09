package com.wavesplatform.generator

import java.io.IOException
import java.net.InetSocketAddress

import cats.Show
import cats.data._
import cats.implicits._
import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.network.RawBytes
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.control.NonFatal

class Worker(settings: Settings, sender: NetworkSender, node: InetSocketAddress, generator: TransactionGenerator, initial: List[RawBytes])(
    implicit ec: ExecutionContext)
    extends ScorexLogging {

  private type Result[T] = EitherT[Future, (Int, Throwable), T]

  def run(): Future[Unit] =
    guardedSend(1).leftMap { _ =>
      ()
    }.merge

  private def guardedSend(startStep: Int): Result[Unit] = {
    tryConnect
      .flatTap { ch =>
        initial match {
          case Nil =>
            ().pure[Result]
          case _ =>
            trySend(ch, initial)
              .leftMap(e => -1 -> e)
        }
      }
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

  private def tryConnect: Result[Channel] = EitherT {
    sender
      .connect(node)
      .map { x =>
        Right(x)
      }
      .recover {
        case NonFatal(e) => Left(0 -> e)
      }
  }

  private def trySend(channel: Channel, messages: Seq[RawBytes]): EitherT[Future, Throwable, Unit] = EitherT {
    sender
      .send(channel, messages: _*)
      .map { _ =>
        Right(())
      }
      .recover {
        case NonFatal(e) => Left(e)
      }
  }

  private def sendTransactions(startStep: Int, channel: Channel): Result[Unit] = {
    def loop(step: Int): Result[Unit] = {
      log.info(s"[$node] Iteration $step")
      val txs = generator.next().toList
      txs.foreach(println)
      val messages: Seq[RawBytes] = txs.map(RawBytes.from).toSeq

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

      trySend(channel, messages)
        .leftMap(e => step -> e)
        .flatMap(_ => next)
    }

    loop(startStep)
  }

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
