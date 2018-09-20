package com.wavesplatform.network

import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.reactive.subjects.ConcurrentSubject

@Sharable
class MessageObserver extends ChannelInboundHandlerAdapter with ScorexLogging {

  implicit val scheduler = monix.execution.Scheduler.fixedPool("message-observer", 2)

  private val transactions = ConcurrentSubject.publish[(Channel, Transaction)]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case tx: Transaction => transactions.onNext((ctx.channel(), tx))
    case _               => super.channelRead(ctx, msg)

  }

  def shutdown(): Unit = {
    transactions.onComplete()
  }
}

object MessageObserver {
  type Messages = ChannelObservable[Transaction]

  def apply(): (MessageObserver, Messages) = {
    val mo = new MessageObserver()
    (mo, mo.transactions)
  }
}
