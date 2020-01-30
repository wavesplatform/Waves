package com.wavesplatform

import java.net.{InetSocketAddress, SocketAddress, URI}
import java.util.concurrent.Callable

import cats.Eq
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.group.{ChannelGroup, ChannelGroupFuture, ChannelMatcher}
import io.netty.channel.local.LocalAddress
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.channel.{Channel, ChannelHandlerContext}
import io.netty.util.NetUtil.toSocketAddressString
import io.netty.util.concurrent.{EventExecutorGroup, ScheduledFuture}
import monix.eval.Coeval
import monix.execution.Scheduler
import monix.reactive.Observable
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Transaction
import kamon.Kamon

import scala.concurrent.duration._

package object network extends ScorexLogging {
  private val broadcastTimeStats = Kamon.timer("network-broadcast-time")

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
    case null                   => ""
    case l: LocalAddress        => s" $l"
    case isa: InetSocketAddress => s" ${toSocketAddressString(isa)}"
    case x                      => s" $x" // For EmbeddedSocketAddress
  }

  def id(ctx: ChannelHandlerContext): String = id(ctx.channel())

  def id(chan: Channel, prefix: String = ""): String =
    if (chan == null) "???" else s"[$prefix${chan.id().asShortText()}${formatAddress(chan.remoteAddress())}]"

  def formatBlocks(blocks: Seq[Block]): String = formatSignatures(blocks.view.map(_.uniqueId))

  def formatSignatures(signatures: Seq[ByteStr]): String =
    if (signatures.isEmpty) "[Empty]"
    else if (signatures.size == 1) s"[${signatures.head.trim}]"
    else s"(total=${signatures.size}) [${signatures.head.trim} -- ${signatures.last.trim}]"

  implicit val channelEq: Eq[Channel] = Eq.fromUniversalEquals

  implicit class ChannelHandlerContextExt(val ctx: ChannelHandlerContext) extends AnyVal {
    def remoteAddress: Option[InetSocketAddress] = ctx.channel() match {
      case x: NioSocketChannel => Option(x.remoteAddress())
      case x =>
        log.debug(s"Doesn't know how to get a remoteAddress from ${id(ctx)}, $x")
        None
    }
  }

  implicit class ChannelGroupExt(val allChannels: ChannelGroup) extends AnyVal {
    def broadcast(message: AnyRef, except: Option[Channel] = None): Unit = broadcast(message, except.toSet)

    def broadcast(message: AnyRef, except: Set[Channel]): ChannelGroupFuture = {
      logBroadcast(message, except)
      val st = broadcastTimeStats.refine("object", message.getClass.getSimpleName).start()
      allChannels
        .writeAndFlush(message, { (channel: Channel) =>
          !except.contains(channel)
        })
        .addListener { _: ChannelGroupFuture =>
          st.stop()
        }
    }

    def broadcastMany(messages: Seq[AnyRef], except: Set[Channel] = Set.empty): Unit = {
      val channelMatcher: ChannelMatcher = { (channel: Channel) =>
        !except.contains(channel)
      }
      messages.foreach { message =>
        logBroadcast(message, except)
        allChannels.write(message, channelMatcher)
      }

      allChannels.flush(channelMatcher)
    }

    def broadcastTx(tx: Transaction, except: Option[Channel] = None): Unit = allChannels.broadcast(RawBytes.from(tx), except)

    def broadcastTx(txs: Seq[Transaction]): Unit = allChannels.broadcastMany(txs.map(RawBytes.from))

    private def logBroadcast(message: AnyRef, except: Set[Channel]): Unit = message match {
      case RawBytes(TransactionSpec.messageCode, _) =>
      case _ =>
        val exceptMsg = if (except.isEmpty) "" else s" (except ${except.map(id(_)).mkString(", ")})"
        log.trace(s"Broadcasting $message to ${allChannels.size()} channels$exceptMsg")
    }
  }

  type ChannelObservable[A] = Observable[(Channel, A)]

  def lastObserved[A](o: Observable[A])(implicit s: Scheduler): Coeval[Option[A]] = {
    @volatile var last = Option.empty[A]
    o.foreach(a => last = Some(a))
    Coeval(last)
  }

  def newItems[A](o: Observable[A])(implicit s: Scheduler): Coeval[Seq[A]] = {
    @volatile var collected = Seq.empty[A]
    o.foreach(a => collected = collected :+ a)
    Coeval {
      val r = collected
      collected = Seq.empty
      r
    }
  }
}
