package com.wavesplatform

import java.net.{InetSocketAddress, SocketAddress, URI}
import java.util.concurrent.Callable

import com.wavesplatform.state2.ByteStr
import io.netty.channel.group.{ChannelGroup, ChannelMatchers}
import io.netty.channel.local.LocalAddress
import io.netty.channel.socket.SocketChannel
import io.netty.channel.{Channel, ChannelHandlerContext}
import io.netty.util.NetUtil.toSocketAddressString
import io.netty.util.concurrent.{EventExecutorGroup, ScheduledFuture}
import kamon.metric.instrument.Histogram
import scorex.block.Block
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

package object network extends ScorexLogging {
  def inetSocketAddress(addr: String, defaultPort: Int): InetSocketAddress = {
    val uri = new URI(s"node://$addr")
    if (uri.getPort < 0) new InetSocketAddress(addr, defaultPort)
    else new InetSocketAddress(uri.getHost, uri.getPort)
  }

  implicit class EventExecutorGroupExt(val e: EventExecutorGroup) extends AnyVal {
    def scheduleWithFixedDelay(initialDelay: FiniteDuration, delay: FiniteDuration)(f: => Unit): ScheduledFuture[_] =
      e.scheduleWithFixedDelay((() => f): Runnable, initialDelay.toNanos, delay.toNanos, NANOSECONDS)

    def schedule[A](delay: FiniteDuration)(f: => A): ScheduledFuture[A] =
      e.schedule((() => f): Callable[A], delay.length, delay.unit)
  }

  private def formatAddress(sa: SocketAddress) = sa match {
    case null => ""
    case l: LocalAddress => s" ${l.toString}"
    case isa: InetSocketAddress => s" ${toSocketAddressString(isa)}"
  }

  def id(ctx: ChannelHandlerContext): String = id(ctx.channel())

  def id(chan: Channel, prefix: String = ""): String = s"[$prefix${chan.id().asShortText()}${formatAddress(chan.remoteAddress())}]"

  def formatBlocks(blocks: Seq[Block]): String = formatSignatures(blocks.view.map(_.uniqueId))

  def formatSignatures(signatures: Seq[ByteStr]): String = if (signatures.isEmpty) ""
  else if (signatures.size == 1) s"[${signatures.head}]"
  else s"[${signatures.head}..${signatures.last}]"

  implicit class ChannelHandlerContextExt(val ctx: ChannelHandlerContext) extends AnyVal {
    def remoteAddress: InetSocketAddress = ctx.channel().asInstanceOf[SocketChannel].remoteAddress()
  }

  implicit class ChannelGroupExt(val allChannels: ChannelGroup) extends AnyVal {
    def broadcast(message: AnyRef, except: Option[Channel] = None): Unit = {
      message match {
        case RawBytes(TransactionMessageSpec.messageCode, _) =>
        case _ => log.trace(s"Broadcasting $message to ${allChannels.size()} channels${except.fold("")(c => s" (except ${id(c)})")}")

      }
      allChannels.writeAndFlush(message, except.fold(ChannelMatchers.all())(ChannelMatchers.isNot))
    }

    def broadcastTx(tx: Transaction, except: Option[Channel] = None): Unit =
      allChannels.broadcast(RawBytes(TransactionMessageSpec.messageCode, tx.bytes), except)
  }

  implicit class HistorgramExt(h: Histogram) {
    def safeRecord(value: Long): Unit = h.record(Math.max(value, 0))
  }

}
