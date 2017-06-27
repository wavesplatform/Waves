package com.wavesplatform

import java.net.{InetSocketAddress, SocketAddress, URI}
import java.util.concurrent.Callable

import io.netty.channel.group.{ChannelGroup, ChannelMatchers}
import io.netty.channel.local.LocalAddress
import io.netty.channel.socket.SocketChannel
import io.netty.channel.{Channel, ChannelHandlerContext}
import io.netty.util.NetUtil.toSocketAddressString
import io.netty.util.concurrent.{EventExecutorGroup, ScheduledFuture}
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

  def id(chan: Channel): String = s"[${chan.id().asShortText()}${formatAddress(chan.remoteAddress())}]"

  implicit class ChannelHandlerContextExt(val ctx: ChannelHandlerContext) extends AnyVal {
    def remoteAddress: InetSocketAddress = ctx.channel().asInstanceOf[SocketChannel].remoteAddress()
  }

  implicit class ChannelExt(val channel: Channel) extends AnyVal {
    def declaredAddress: Option[InetSocketAddress] = Option(channel.attr(AttributeKeys.DeclaredAddress).get())
  }

  implicit class ChannelGroupExt(val allChannels: ChannelGroup) extends AnyVal {
    def broadcast(message: AnyRef, except: Option[Channel] = None): Unit = {
      log.debug(s"Broadcasting $message to ${allChannels.size()} channels${except.fold("")(c => s" (except ${id(c)})")}")
      allChannels.writeAndFlush(message, except.fold(ChannelMatchers.all())(ChannelMatchers.isNot))
    }
  }

}
