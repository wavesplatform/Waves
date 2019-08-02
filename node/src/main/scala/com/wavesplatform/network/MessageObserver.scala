package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.{Schedulers, ScorexLogging}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.reactive.subjects.ConcurrentSubject

@Sharable
final class MessageObserver extends ChannelInboundHandlerAdapter with ScorexLogging {
  private[this] implicit val scheduler = Schedulers.fixedPool(2, "message-observer")

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit =
    subjects.processMessage(ctx.channel()).applyOrElse(msg, super.channelRead(ctx, _))

  def shutdown(): Unit =
    subjects.completeAll()

  //noinspection ScalaStyle,TypeAnnotation
  private object subjects {
    val signatures = ConcurrentSubject.publish[(Channel, Signatures)]
    val blocks = ConcurrentSubject.publish[(Channel, Block)]
    val blockchainScores = ConcurrentSubject.publish[(Channel, BigInt)]
    val microblockInvs = ConcurrentSubject.publish[(Channel, MicroBlockInv)]
    val microblockResponses = ConcurrentSubject.publish[(Channel, MicroBlockResponse)]
    val transactions = ConcurrentSubject.publish[(Channel, Transaction)]

    def processMessage(channel: Channel): PartialFunction[AnyRef, Unit] = {
      case b: Block => subjects.blocks.onNext((channel, b))
      case sc: BigInt => subjects.blockchainScores.onNext((channel, sc))
      case s: Signatures => subjects.signatures.onNext((channel, s))
      case mbInv: MicroBlockInv => subjects.microblockInvs.onNext((channel, mbInv))
      case mb: MicroBlockResponse => subjects.microblockResponses.onNext((channel, mb))
      case tx: Transaction => subjects.transactions.onNext((channel, tx))
    }

    def completeAll(): Unit = {
      signatures.onComplete()
      blocks.onComplete()
      blockchainScores.onComplete()
      microblockInvs.onComplete()
      microblockResponses.onComplete()
      transactions.onComplete()
    }
  }
}

object MessageObserver {
  type Messages = (ChannelObservable[Signatures],
                   ChannelObservable[Block],
                   ChannelObservable[BigInt],
                   ChannelObservable[MicroBlockInv],
                   ChannelObservable[MicroBlockResponse],
                   ChannelObservable[Transaction])

  def apply(): (MessageObserver, Messages) = {
    val mo = new MessageObserver()
    (mo, (mo.subjects.signatures, mo.subjects.blocks, mo.subjects.blockchainScores, mo.subjects.microblockInvs, mo.subjects.microblockResponses, mo.subjects.transactions))
  }
}
