package com.wavesplatform.database

import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.utils.Time

import java.util
import java.util.concurrent.{AbstractExecutorService, TimeUnit}

object TestStorageFactory {
  private val runNow = new AbstractExecutorService {
    override def shutdown(): Unit                                         = {}
    override def shutdownNow(): util.List[Runnable]                       = util.Collections.emptyList()
    override def isShutdown: Boolean                                      = false
    override def isTerminated: Boolean                                    = false
    override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true
    override def execute(command: Runnable): Unit                         = command.run()
  }

  def apply(
      settings: WavesSettings,
      rdb: RDB,
      time: Time,
      blockchainUpdateTriggers: BlockchainUpdateTriggers
  ): (BlockchainUpdaterImpl, RocksDBWriter) = {
    val rocksDBWriter: RocksDBWriter =
      RocksDBWriter(rdb, settings.blockchainSettings, settings.dbSettings, settings.enableLightMode, 100, Some(runNow))
    (
      new BlockchainUpdaterImpl(rocksDBWriter, settings, time, blockchainUpdateTriggers, loadActiveLeases(rdb, _, _)),
      rocksDBWriter
    )
  }
}
