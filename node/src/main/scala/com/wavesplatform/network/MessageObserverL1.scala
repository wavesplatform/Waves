package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.network.MessageObserverL1.Messages
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.Schedulers
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.execution.schedulers.SchedulerService
import monix.reactive.subjects.ConcurrentSubject

@Sharable
class MessageObserverL1 extends ChannelInboundHandlerAdapter {

  private implicit val scheduler: SchedulerService = Schedulers.fixedPool(2, "message-observer")

  private val signatures          = ConcurrentSubject.publish[(Channel, Signatures)]
  private val blocks              = ConcurrentSubject.publish[(Channel, Block)]
  private val blockchainScores    = ConcurrentSubject.publish[(Channel, BigInt)]
  private val microblockInvs      = ConcurrentSubject.publish[(Channel, MicroBlockInv)]
  private val microblockResponses = ConcurrentSubject.publish[(Channel, MicroBlockResponse)]
  private val transactions        = ConcurrentSubject.publish[(Channel, Transaction)]
  private val blockSnapshots      = ConcurrentSubject.publish[(Channel, BlockSnapshotResponse)]
  private val microblockSnapshots = ConcurrentSubject.publish[(Channel, MicroBlockSnapshotResponse)]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case b: Block                       => blocks.onNext((ctx.channel(), b))
    case sc: BigInt                     => blockchainScores.onNext((ctx.channel(), sc))
    case s: Signatures                  => signatures.onNext((ctx.channel(), s))
    case mbInv: MicroBlockInv           => microblockInvs.onNext((ctx.channel(), mbInv))
    case mb: MicroBlockResponse         => microblockResponses.onNext((ctx.channel(), mb))
    case tx: Transaction                => transactions.onNext((ctx.channel(), tx))
    case sn: BlockSnapshotResponse      => blockSnapshots.onNext((ctx.channel(), sn))
    case sn: MicroBlockSnapshotResponse => microblockSnapshots.onNext((ctx.channel(), sn))
    case _                              => super.channelRead(ctx, msg)

  }

  def messages: Messages = {
    (
      signatures,
      blocks,
      blockchainScores,
      microblockInvs,
      microblockResponses,
      transactions,
      blockSnapshots,
      microblockSnapshots
    )
  }

  def shutdown(): Unit = {
    signatures.onComplete()
    blocks.onComplete()
    blockchainScores.onComplete()
    microblockInvs.onComplete()
    microblockResponses.onComplete()
    transactions.onComplete()
    blockSnapshots.onComplete()
    microblockSnapshots.onComplete()
  }
}

object MessageObserverL1 {
  type Messages = (
      ChannelObservable[Signatures],
      ChannelObservable[Block],
      ChannelObservable[BigInt],
      ChannelObservable[MicroBlockInv],
      ChannelObservable[MicroBlockResponse],
      ChannelObservable[Transaction],
      ChannelObservable[BlockSnapshotResponse],
      ChannelObservable[MicroBlockSnapshotResponse]
  )
}
