package scorex.transaction.state.database.blockchain

import akka.actor.Actor
import scorex.block.Block.BlockId
import scorex.transaction.BlockStorage
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


/**
  * Delete old states / blocks we don't need any more
  */
class HistoryScavenger(blockStorage: BlockStorage) extends Actor with ScorexLogging {

  context.system.scheduler.schedule(1.minute, 5.minutes, self, Unit)

  override def receive: Receive = {
    case Unit =>
      log.info("Remove old states")
      val lastBlocks: Set[BlockId] = blockStorage.history.lastBlocks(blockStorage.MaxRollback).map(_.uniqueId).toSet
      val storedStates: Set[BlockId] = blockStorage.stateHistory.keySet
      val diff = storedStates.diff(lastBlocks)
      diff.foreach(id => blockStorage.stateHistory.removeState(id))
  }
}
