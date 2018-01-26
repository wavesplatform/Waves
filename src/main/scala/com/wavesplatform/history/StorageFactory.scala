package com.wavesplatform.history

import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import com.wavesplatform.utils.HeightInfo
import monix.eval.Coeval
import org.iq80.leveldb.DB
import scorex.transaction._
import scorex.utils.{NTP, Time}

import scala.util.{Success, Try}

object StorageFactory {

  type Storage = Coeval[(NgHistory with DebugNgHistory, FeatureProvider, StateReader, BlockchainUpdater, BlockchainDebugInfo)]
  type HeightInfos = Coeval[(HeightInfo, HeightInfo)]

  private def createStateStorage(history: History with FeatureProvider, db: DB, time: Time): Try[StateStorage] =
    StateStorage(db, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        StateStorage(db, dropExisting = true)
      }
    }

  def apply[T](db: DB, settings: WavesSettings, time: Time = NTP): Try[(Storage, HeightInfos)] = {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(db, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
      ss <- createStateStorage(historyWriter, db, time)
    } yield (
      Coeval {
        val stateWriter = new StateWriterImpl(ss, lock)
        val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, time, lock)
        val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
        (history, history, bcu.bestLiquidState, bcu, bcu)
      },
      Coeval {
        (historyWriter.debugInfo, ss.debugInfo)
      }
    )
  }
}
