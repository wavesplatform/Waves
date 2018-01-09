package com.wavesplatform.history

import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import org.iq80.leveldb.DB
import scorex.transaction._
import scorex.utils.NTP

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History with FeatureProvider, db: DB): Try[StateStorage] =
    StateStorage(db, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        StateStorage(db, dropExisting = true)
      }
    }

  def apply[T](db: DB, settings: WavesSettings,
               beforeStateUpdate: (BlockchainUpdater, StateWriter) => T = (_: BlockchainUpdater, _: StateWriter) => (),
               time: scorex.utils.Time = NTP):
  Try[(NgHistory with DebugNgHistory, FeatureProvider, StateReader, BlockchainUpdater, BlockchainDebugInfo, T)] = {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(db, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
      ss <- createStateStorage(historyWriter, db)
      stateWriter = new StateWriterImpl(ss, lock)
      bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, time, lock)
      callbackResult = beforeStateUpdate(bcu, stateWriter)
    } yield {
      bcu.syncPersistedAndInMemory()
      val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
      (history, history, bcu.bestLiquidState, bcu, bcu, callbackResult)
    }
  }
}
