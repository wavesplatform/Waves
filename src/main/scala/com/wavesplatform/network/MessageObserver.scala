package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.reactive.subjects.ConcurrentSubject
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.{History, Transaction}
import scorex.utils.ScorexLogging

@Sharable
class MessageObserver extends ChannelInboundHandlerAdapter with ScorexLogging {

  implicit val scheduler = monix.execution.Scheduler.fixedPool("message-observer", 2)

  private val signatures          = ConcurrentSubject.publish[(Channel, Signatures)]
  private val blocks              = ConcurrentSubject.publish[(Channel, Block)]
  private val checkpoints         = ConcurrentSubject.publish[(Channel, Checkpoint)]
  private val blockchainScores    = ConcurrentSubject.publish[(Channel, BlockchainScore)]
  private val microblockInvs      = ConcurrentSubject.publish[(Channel, MicroBlockInv)]
  private val microblockResponses = ConcurrentSubject.publish[(Channel, MicroBlockResponse)]
  private val transactions        = ConcurrentSubject.publish[(Channel, Transaction)]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case b: Block                    => blocks.onNext((ctx.channel(), b))
    case sc: History.BlockchainScore => blockchainScores.onNext((ctx.channel(), sc))
    case s: Signatures               => signatures.onNext((ctx.channel(), s))
    case c: Checkpoint               => checkpoints.onNext((ctx.channel(), c))
    case mbInv: MicroBlockInv        => microblockInvs.onNext((ctx.channel(), mbInv))
    case mb: MicroBlockResponse      => microblockResponses.onNext((ctx.channel(), mb))
    case tx: Transaction             => transactions.onNext((ctx.channel(), tx))
    case _                           => super.channelRead(ctx, msg)

  }

  def shutdown(): Unit = {
    signatures.onComplete()
    blocks.onComplete()
    checkpoints.onComplete()
    blockchainScores.onComplete()
    microblockInvs.onComplete()
    microblockResponses.onComplete()
    transactions.onComplete()
  }
}

object MessageObserver {
  type Messages = (ChannelObservable[Signatures],
                   ChannelObservable[Block],
                   ChannelObservable[BlockchainScore],
                   ChannelObservable[Checkpoint],
                   ChannelObservable[MicroBlockInv],
                   ChannelObservable[MicroBlockResponse],
                   ChannelObservable[Transaction])

  def apply(): (MessageObserver, Messages) = {
    val mo = new MessageObserver()
    (mo, (mo.signatures, mo.blocks, mo.blockchainScores, mo.checkpoints, mo.microblockInvs, mo.microblockResponses, mo.transactions))
  }
}
