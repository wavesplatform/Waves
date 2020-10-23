//package com.wavesplatform.events
//
//import com.wavesplatform.block.{Block, MicroBlock}
//import com.wavesplatform.common.utils.EitherExt2
//import com.wavesplatform.mining.MiningConstraint
//import com.wavesplatform.state.diffs.BlockDiffer
//import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
//import monix.execution.Scheduler.Implicits.global
//import monix.reactive.subjects.ReplaySubject
//import org.scalatest.Suite
//
//import scala.concurrent.duration._
//
//private[events] trait EventsHelpers extends WithBlockchain { _: Suite =>
//  protected def produceEvent(useTrigger: BlockchainUpdateTriggers => Unit): BlockchainUpdated = {
//    val evts = ReplaySubject[BlockchainUpdated]()
//    val t    = new BlockchainUpdateTriggersImpl(evts)
//    useTrigger(t)
//    evts.onComplete()
//    evts.toListL.runSyncUnsafe(500.milliseconds).head
//  }
//
//  protected def detailedDiffFromBlock(b: Block): DetailedDiff =
//    BlockDiffer.fromBlock(blockchain, None, b, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff
//
//  protected def appendBlock(b: Block, minerReward: Option[Long] = None): BlockAppended =
//    produceEvent(_.onProcessBlock(b, detailedDiffFromBlock(b), minerReward, blockchain)) match {
//      case ba: BlockAppended => ba
//      case _                 => fail()
//    }
//
//  protected def appendMicroBlock(mb: MicroBlock): MicroBlockAppended = {
//    val dd = BlockDiffer.fromMicroBlock(blockchain, Some(0), mb, 1, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff
//    produceEvent(_.onProcessMicroBlock(mb, dd, blockchain, mb.totalResBlockSig)) match {
//      case mba: MicroBlockAppended => mba
//      case _                       => fail()
//    }
//  }
//}
