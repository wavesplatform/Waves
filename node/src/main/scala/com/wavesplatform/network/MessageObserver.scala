package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.{Schedulers, ScorexLogging}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.execution.schedulers.SchedulerService
import monix.reactive.subjects.ConcurrentSubject

@Sharable
class MessageObserver extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val scheduler: SchedulerService = Schedulers.fixedPool(2, "message-observer")

  private val signatures          = ConcurrentSubject.publish[(Channel, Signatures)]
  private val blocks              = ConcurrentSubject.publish[(Channel, Block)]
  private val blockchainScores    = ConcurrentSubject.publish[(Channel, BigInt)]
  private val microblockInvs      = ConcurrentSubject.publish[(Channel, MicroBlockInv)]
  private val microblockResponses = ConcurrentSubject.publish[(Channel, MicroBlockResponse)]
  private val transactions        = ConcurrentSubject.publish[(Channel, Transaction)]
  private val snapshots           = ConcurrentSubject.publish[(Channel, Snapshots)]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case b: Block               => blocks.onNext((ctx.channel(), b))
    case sc: BigInt             => blockchainScores.onNext((ctx.channel(), sc))
    case s: Signatures          => signatures.onNext((ctx.channel(), s))
    case mbInv: MicroBlockInv   => microblockInvs.onNext((ctx.channel(), mbInv))
    case mb: MicroBlockResponse => microblockResponses.onNext((ctx.channel(), mb))
    case tx: Transaction        => transactions.onNext((ctx.channel(), tx))
    case sn: Snapshots          => snapshots.onNext((ctx.channel(), sn))
    case _                      => super.channelRead(ctx, msg)

  }

  def shutdown(): Unit = {
    signatures.onComplete()
    blocks.onComplete()
    blockchainScores.onComplete()
    microblockInvs.onComplete()
    microblockResponses.onComplete()
    transactions.onComplete()
    snapshots.onComplete()
  }
}

object MessageObserver {
  type Messages = (
      ChannelObservable[Signatures],
      ChannelObservable[Block],
      ChannelObservable[BigInt],
      ChannelObservable[MicroBlockInv],
      ChannelObservable[MicroBlockResponse],
      ChannelObservable[Transaction],
      ChannelObservable[Snapshots]
  )

  def apply(): (MessageObserver, Messages) = {
    val mo = new MessageObserver()
    (mo, (mo.signatures, mo.blocks, mo.blockchainScores, mo.microblockInvs, mo.microblockResponses, mo.transactions, mo.snapshots))
  }
}
