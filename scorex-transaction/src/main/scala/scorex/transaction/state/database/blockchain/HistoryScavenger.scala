package scorex.transaction.state.database.blockchain

import akka.actor.Actor
import scorex.crypto.encode.Base58
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
      val lastBlocks = blockStorage.history.lastBlocks(blockStorage.MaxRollback).map(b => Base58.encode(b.uniqueId)).toSet
      val storedStates = blockStorage.stateHistory.keySet.map(Base58.encode)
      val diff = storedStates.diff(lastBlocks)
      log.info("Remove old states: " + diff)
      diff.flatMap(en => Base58.decode(en).toOption).foreach(id => blockStorage.stateHistory.removeState(id))
  }
}
