package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.Schedulers
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.execution.schedulers.SchedulerService
import monix.reactive.subjects.ConcurrentSubject

@Sharable
class MessageObserver extends ChannelInboundHandlerAdapter {

  private implicit val scheduler: SchedulerService = Schedulers.fixedPool(2, "message-observer")

  private val signaturesSubj                    = ConcurrentSubject.publish[(Channel, Signatures)]
  val signatures: ChannelObservable[Signatures] = signaturesSubj

  private val blocksSubj               = ConcurrentSubject.publish[(Channel, Block)]
  val blocks: ChannelObservable[Block] = blocksSubj

  private val blockchainScoresSubj                = ConcurrentSubject.publish[(Channel, BigInt)]
  val blockchainScores: ChannelObservable[BigInt] = blockchainScoresSubj

  private val microblockInvsSubj                       = ConcurrentSubject.publish[(Channel, MicroBlockInv)]
  val microblockInvs: ChannelObservable[MicroBlockInv] = microblockInvsSubj

  private val microblockResponsesSubj                            = ConcurrentSubject.publish[(Channel, MicroBlockResponse)]
  val microblockResponses: ChannelObservable[MicroBlockResponse] = microblockResponsesSubj

  private val transactionsSubj                     = ConcurrentSubject.publish[(Channel, Transaction)]
  val transactions: ChannelObservable[Transaction] = transactionsSubj

  private val blockSnapshotsSubj                               = ConcurrentSubject.publish[(Channel, BlockSnapshotResponse)]
  val blockSnapshots: ChannelObservable[BlockSnapshotResponse] = blockSnapshotsSubj

  private val microblockSnapshotsSubj                                    = ConcurrentSubject.publish[(Channel, MicroBlockSnapshotResponse)]
  val microblockSnapshots: ChannelObservable[MicroBlockSnapshotResponse] = microblockSnapshotsSubj

  private val endorseBlocksSubj                      = ConcurrentSubject.publish[(Channel, EndorseBlock)]
  val endorseBlocks: ChannelObservable[EndorseBlock] = endorseBlocksSubj

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case b: Block                       => blocksSubj.onNext((ctx.channel(), b))
    case sc: BigInt                     => blockchainScoresSubj.onNext((ctx.channel(), sc))
    case s: Signatures                  => signaturesSubj.onNext((ctx.channel(), s))
    case mbInv: MicroBlockInv           => microblockInvsSubj.onNext((ctx.channel(), mbInv))
    case mb: MicroBlockResponse         => microblockResponsesSubj.onNext((ctx.channel(), mb))
    case tx: Transaction                => transactionsSubj.onNext((ctx.channel(), tx))
    case sn: BlockSnapshotResponse      => blockSnapshotsSubj.onNext((ctx.channel(), sn))
    case sn: MicroBlockSnapshotResponse => microblockSnapshotsSubj.onNext((ctx.channel(), sn))
    case e: EndorseBlock                => endorseBlocksSubj.onNext((ctx.channel(), e))
    case _                              => super.channelRead(ctx, msg)
  }

  def shutdown(): Unit = {
    signaturesSubj.onComplete()
    blocksSubj.onComplete()
    blockchainScoresSubj.onComplete()
    microblockInvsSubj.onComplete()
    microblockResponsesSubj.onComplete()
    transactionsSubj.onComplete()
    blockSnapshotsSubj.onComplete()
    microblockSnapshotsSubj.onComplete()
    endorseBlocksSubj.onComplete()
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
      ChannelObservable[BlockSnapshotResponse],
      ChannelObservable[MicroBlockSnapshotResponse]
  )
}
