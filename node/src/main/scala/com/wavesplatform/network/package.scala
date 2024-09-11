package com.wavesplatform

import cats.Eq
import com.typesafe.scalalogging.Logger
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Transaction
import io.netty.channel.group.{ChannelGroup, ChannelGroupFuture}
import io.netty.channel.local.LocalAddress
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.channel.{Channel, ChannelHandlerContext}
import io.netty.util.NetUtil.toSocketAddressString
import io.netty.util.concurrent.{EventExecutorGroup, ScheduledFuture}
import kamon.Kamon
import monix.eval.Coeval
import monix.execution.Scheduler
import monix.reactive.Observable
import org.slf4j.LoggerFactory

import java.net.{InetAddress, InetSocketAddress, SocketAddress, URI}
import java.util.concurrent.Callable
import scala.concurrent.duration.*

package object network {
  private val broadcastTimeStats = Kamon.timer("network-broadcast-time")
  private lazy val logger: Logger =
    Logger(LoggerFactory.getLogger(getClass.getName))

  def inetSocketAddress(addr: String, defaultPort: Int): Seq[InetSocketAddress] = {
    val uri        = new URI(s"node://$addr")
    val actualPort = if (uri.getPort > 0) uri.getPort else defaultPort
    InetAddress.getAllByName(uri.getHost).map { ia =>
      new InetSocketAddress(ia, actualPort)
    }
  }

  implicit class EventExecutorGroupExt(val e: EventExecutorGroup) extends AnyVal {
    def scheduleWithFixedDelay(initialDelay: FiniteDuration, delay: FiniteDuration)(f: => Unit): ScheduledFuture[?] =
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
    if (chan == null) "[null]" else s"[$prefix${chan.id().asShortText()}${formatAddress(chan.remoteAddress())}]"

  def formatBlocks(blocks: Seq[Block]): String = formatSignatures(blocks.view.map(_.id()))

  def formatSignatures(signatures: Iterable[ByteStr]): String =
    if (signatures.isEmpty) "[Empty]"
    else if (signatures.sizeCompare(1) <= 0) s"[${signatures.head.trim}]"
    else s"(total=${signatures.size}) [${signatures.head.trim} -- ${signatures.last.trim}]"

  implicit val channelEq: Eq[Channel] = Eq.fromUniversalEquals

  implicit class ChannelHandlerContextExt(val ctx: ChannelHandlerContext) extends AnyVal {
    def remoteAddress: Option[InetSocketAddress] = ctx.channel() match {
      case x: NioSocketChannel => Option(x.remoteAddress())
      case x =>
        logger.debug(s"Doesn't know how to get a remoteAddress from ${id(ctx)}, $x")
        None
    }
  }

  implicit class ChannelGroupExt(val allChannels: ChannelGroup) extends AnyVal {
    def broadcast(message: AnyRef, except: Option[Channel] = None): Unit = broadcast(message, except.toSet)

    def broadcast(message: AnyRef, except: Set[Channel]): ChannelGroupFuture = {
      logBroadcast(message, except)
      val st = broadcastTimeStats.withTag("object", message.getClass.getSimpleName).start()
      allChannels
        .writeAndFlush(
          message,
          { (channel: Channel) =>
            !except.contains(channel)
          }
        )
        .addListener { (_: ChannelGroupFuture) =>
          st.stop()
        }
    }

    private def logBroadcast(message: AnyRef, except: Set[Channel]): Unit = message match {
      case RawBytes(TransactionSpec.messageCode | PBTransactionSpec.messageCode, _) =>
      case _ =>
        logger.trace {
          val exceptMsg = if (except.isEmpty) "" else s" (except ${except.map(id(_)).mkString(", ")})"
          val msgString = message match {
            case t: Transaction => s"transaction ${t.id()}"
            case BlockForged(b) => s"block ${b.id()}"
            case other          => other.toString
          }
          s"Broadcasting $msgString to ${allChannels.size()} channels$exceptMsg"
        }
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
