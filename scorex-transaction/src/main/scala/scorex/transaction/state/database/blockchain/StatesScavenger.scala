package scorex.transaction.state.database.blockchain

import akka.actor.Actor
import scorex.crypto.encode.Base58
import scorex.transaction.BlockStorage
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

/**
  * Delete old states / blocks we don't need any more
  */
class StatesScavenger(blockStorage: BlockStorage) extends Actor with ScorexLogging {

  context.system.scheduler.schedule(1.minute, 5.minutes, self, Unit)

  override def receive: Receive = {
    case Unit =>
      val h = blockStorage.history
      val diff = blockStorage.stateHistory.keySet.filter { id =>
        Try {
          h.height() - h.heightOf(Base58.decode(id).get).get > blockStorage.MaxRollback
        }.getOrElse(false)
      }
      log.info("Remove old states: " + diff)
      diff.foreach(id => blockStorage.stateHistory.removeState(id))
  }
}
