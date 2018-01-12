package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import com.wavesplatform.utils.HeightInfo
import monix.eval.Coeval
import scorex.transaction._
import scorex.utils.{NTP, Time}

import scala.util.{Success, Try}

object StorageFactory {

  type Storage = Coeval[(NgHistory with DebugNgHistory with AutoCloseable, FeatureProvider, AutoCloseable, StateReader, BlockchainUpdater, BlockchainDebugInfo)]
  type HeightInfos = Coeval[(HeightInfo, HeightInfo)]

  private def createStateStorage(history: History with FeatureProvider, stateFile: Option[File], pageSplitSize: Int, time: Time): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false, time).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true, time)
      }
    }

  def apply[T](settings: WavesSettings, time: Time = NTP): Try[(Storage, HeightInfos)] = {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainSettings.blockchainFile, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings, time)
      ss <- createStateStorage(historyWriter, settings.blockchainSettings.stateFile, settings.mvstorePageSplitSize, time)
    } yield (
      Coeval {
        val stateWriter = new StateWriterImpl(ss, settings.blockchainSettings.storeTransactionsInState, lock)
        val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, time, lock)
        val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
        (history, history, stateWriter, bcu.bestLiquidState, bcu, bcu)
      },
      Coeval { (historyWriter.debugInfo, ss.debugInfo) }
    )
  }
}
