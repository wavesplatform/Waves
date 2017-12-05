package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.block.Block
import scorex.network.message.{Message => ScorexMessage}
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging

@Sharable
class TrafficLogger(settings: TrafficLogger.Settings) extends ChannelDuplexHandler with ScorexLogging {

  import BasicMessagesRepo.specsByClasses

  private val codeOf: AnyRef => Option[Byte] = {
    val aux: PartialFunction[AnyRef, Byte] = {
      case x: RawBytes => x.code
      case _: Transaction => TransactionMessageSpec.messageCode
      case _: BigInt | _: LocalScoreChanged => ScoreMessageSpec.messageCode
      case _: Block | _: BlockForged => BlockMessageSpec.messageCode
      case x: Message => specsByClasses(x.getClass).messageCode
      case _: Handshake => HandshakeMessageSpec.messageCode
    }

    aux.lift
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    codeOf(msg).filterNot(settings.ignoreTxMessages).foreach { code =>
      log.trace(s"${id(ctx)} <-- transmitted($code): $msg")
    }

    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    codeOf(msg).filterNot(settings.ignoreRxMessages).foreach { code =>
      log.trace(s"${id(ctx)} --> received($code): $msg")
    }

    super.channelRead(ctx, msg)
  }

}

object TrafficLogger {

  case class Settings(ignoreTxMessages: Set[ScorexMessage.MessageCode],
                      ignoreRxMessages: Set[ScorexMessage.MessageCode])

}