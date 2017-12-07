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

  def apply(db: DB, settings: WavesSettings, time: scorex.utils.Time = NTP): Try[(NgHistory with DebugNgHistory, FeatureProvider, StateReader, BlockchainUpdater, BlockchainDebugInfo)] = {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(db, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
      ss <- createStateStorage(historyWriter, db)
      stateWriter = new StateWriterImpl(ss, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, time, lock)
      val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
      (history, history, bcu.bestLiquidState, bcu, bcu)
    }
  }
}
