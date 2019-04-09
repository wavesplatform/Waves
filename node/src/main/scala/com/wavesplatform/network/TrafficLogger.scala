package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import com.wavesplatform.block.Block
import com.wavesplatform.network.message.{Message => ScorexMessage}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.transaction.Transaction

@Sharable
class TrafficLogger(settings: TrafficLogger.Settings) extends ChannelDuplexHandler with ScorexLogging {

  import BasicMessagesRepo.specsByClasses

  private val codeOf: AnyRef => Option[Byte] = {
    val aux: PartialFunction[AnyRef, Byte] = {
      case x: RawBytes                      => x.code
      case _: Transaction                   => TransactionSpec.messageCode
      case _: BigInt | _: LocalScoreChanged => ScoreSpec.messageCode
      case _: Block | _: BlockForged        => BlockSpec.messageCode
      case x: Message                       => specsByClasses(x.getClass).messageCode
      case _: Handshake                     => HandshakeSpec.messageCode
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

  case class Settings(ignoreTxMessages: Set[ScorexMessage.MessageCode], ignoreRxMessages: Set[ScorexMessage.MessageCode])

}
