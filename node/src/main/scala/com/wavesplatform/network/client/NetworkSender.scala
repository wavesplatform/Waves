package com.wavesplatform.network.client

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.channels.ClosedChannelException

import com.wavesplatform.network.TrafficLogger
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor

import scala.concurrent.{ExecutionContext, Future, Promise}

class NetworkSender(trafficLoggerSettings: TrafficLogger.Settings, chainId: Char, name: String, nonce: Long)(implicit ec: ExecutionContext) extends ScorexLogging {

  private val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  private val client      = new NetworkClient(trafficLoggerSettings, chainId, name, nonce, allChannels)

  def connect(address: InetSocketAddress): Future[Channel] = {
    client.connect(address)
  }

  def send(channel: Channel, messages: Any*): Future[Unit] = {
    if (messages.isEmpty) return Future.successful(())

    if (channel.isOpen) {
      def write(messages: Seq[Any]): Future[Unit] = {
        val (send, keep) = messages.splitAt(1000)
        val futures = send.map { msg =>
          val result = Promise[Unit]()
          channel.write(msg).addListener { (f: io.netty.util.concurrent.Future[Void]) =>
            if (!f.isSuccess) {
              val cause = Option(f.cause()).getOrElse(new IOException("Can't send a message to the channel"))
              log.error(s"Can't send a message to the channel: $msg", cause)
              result.failure(cause)
            } else {
              result.success(())
            }
          }
          result.future
        }

        channel.flush()
        Future.sequence(futures).flatMap(_ => write(keep))
      }

      write(messages)
    } else {
      Future.failed(new ClosedChannelException)
    }
  }

  def close(): Unit = client.shutdown()
}
