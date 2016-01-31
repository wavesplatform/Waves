package scorex.transaction.state.database.blockchain

import akka.actor.Actor
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
      val lastBlocks = blockStorage.history.lastBlocks(blockStorage.MaxRollback).map(b => b.encodedId).toSet
      val storedStates = blockStorage.stateHistory.keySet
      val diff = storedStates.diff(lastBlocks)
      log.info("Remove old states: " + diff)
      diff.foreach(id => blockStorage.stateHistory.removeState(id))
  }
}
