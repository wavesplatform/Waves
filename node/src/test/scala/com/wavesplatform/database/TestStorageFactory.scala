package com.wavesplatform.database

import com.google.common.hash.{Funnels, BloomFilter as GBloomFilter}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.utils.Time
import org.rocksdb.RocksDB

object TestStorageFactory {
  private def wrappedFilter(use: Boolean): BloomFilter =
    if (use) new Wrapper(GBloomFilter.create(Funnels.byteArrayFunnel(), 1000L)) else BloomFilter.AlwaysEmpty

  def apply(
      settings: WavesSettings,
      db: RocksDB,
      time: Time,
      blockchainUpdateTriggers: BlockchainUpdateTriggers
  ): (BlockchainUpdaterImpl, RocksDBWriter) = {
    val useBloomFilter = settings.dbSettings.useBloomFilter
    val rocksDBWriter: RocksDBWriter = new RocksDBWriter(db, 200, settings.blockchainSettings, settings.dbSettings) {
      override val dataKeyFilter: BloomFilter = wrappedFilter(useBloomFilter)
      override val addressFilter: BloomFilter = wrappedFilter(useBloomFilter)
    }
    (
      new BlockchainUpdaterImpl(rocksDBWriter, settings, time, blockchainUpdateTriggers, loadActiveLeases(db, _, _)),
      rocksDBWriter
    )
  }
}
