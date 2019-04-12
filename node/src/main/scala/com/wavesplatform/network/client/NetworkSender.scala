package com.wavesplatform.network.client

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.channels.ClosedChannelException

import com.wavesplatform.network.RawBytes
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor

import scala.concurrent.{ExecutionContext, Future, Promise}

object NetworkSender {
  sealed trait Serializable {
    def bytes: RawBytes
  }

  object Serializable {
    case class TX(tx: Transaction) extends Serializable {
      def bytes = RawBytes.from(tx)
    }

    case class Raw(bytes: RawBytes) extends Serializable
  }
}

class NetworkSender(chainId: Char, name: String, nonce: Long)(implicit ec: ExecutionContext) extends ScorexLogging {

  private val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  private val client      = new NetworkClient(chainId, name, nonce, allChannels)

  def connect(address: InetSocketAddress): Future[Channel] = {
    client.connect(address)
  }

  def send(channel: Channel, messages: NetworkSender.Serializable*): Future[Unit] = {
    if (messages.isEmpty) return Future.successful(())

    if (channel.isOpen) {
      def write(messages: Seq[NetworkSender.Serializable]): Future[Unit] = messages match {
        case msg +: rest =>
          val result = Promise[Unit]()
          channel.write(msg.bytes).addListener { (f: io.netty.util.concurrent.Future[Void]) =>
            if (!f.isSuccess) {
              val cause = Option(f.cause()).getOrElse(new IOException("Can't send a message to the channel"))
              log.error(s"Can't send a message to the channel: $msg", cause)
              result.failure(cause)
            } else {
              log.debug(s"Sent message to a channel $channel: $msg")
              result.success(())
            }
          }
          channel.flush()
          result.future.flatMap(_ => write(rest))

        case Nil =>
          Future.successful(())
      }

      write(messages)
    } else {
      Future.failed(new ClosedChannelException)
    }
  }

  def close(): Unit = client.shutdown()
}
