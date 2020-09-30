package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.network.BasicMessagesRepo.specsByCodes
import com.wavesplatform.network.message.{Message => ScorexMessage}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}

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
      log.trace(s"${id(ctx)} <-- transmitted($code): ${stringify(msg)}")
    }

    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    codeOf(msg).filterNot(settings.ignoreRxMessages).foreach { code =>
      log.trace(s"${id(ctx)} --> received($code): ${stringify(msg)}")
    }

    super.channelRead(ctx, msg)
  }

  private def stringify(msg: Any): String = msg match {
    case tx: Transaction    => s"Transaction(${tx.id()})"
    case b: Block           => s"${b.id()}, header: ${BlockHeaderSerializer.toJson(b.header, b.bytes().length, b.transactionData.length, b.signature).toString}"
    case RawBytes(code, data) => s"RawBytes(${specsByCodes(code).messageName}, ${data.length} bytes)"
    case other              => other.toString
  }
}

object TrafficLogger {

  case class Settings(ignoreTxMessages: Set[ScorexMessage.MessageCode], ignoreRxMessages: Set[ScorexMessage.MessageCode])

}
