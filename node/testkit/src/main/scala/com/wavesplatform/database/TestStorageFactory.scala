package com.wavesplatform.database

import com.google.common.hash.{Funnels, BloomFilter => GBloomFilter}
import com.wavesplatform.account.Address
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.Time
import monix.reactive.Observer
import org.iq80.leveldb.DB

object TestStorageFactory {
  private def wrappedFilter(): BloomFilter = new Wrapper(GBloomFilter.create(Funnels.byteArrayFunnel(), 1000L))

  def apply(
      settings: WavesSettings,
      db: DB,
      time: Time,
      spendableBalanceChanged: Observer[(Address, Asset)],
      blockchainUpdateTriggers: BlockchainUpdateTriggers
  ): (BlockchainUpdaterImpl, LevelDBWriter) = {
    val levelDBWriter: LevelDBWriter = new LevelDBWriter(db, spendableBalanceChanged, settings.blockchainSettings, settings.dbSettings) {
      override val orderFilter: BloomFilter        = wrappedFilter()
      override val dataKeyFilter: BloomFilter      = wrappedFilter()
      override val wavesBalanceFilter: BloomFilter = wrappedFilter()
      override val assetBalanceFilter: BloomFilter = wrappedFilter()
    }
    (
      new BlockchainUpdaterImpl(levelDBWriter, spendableBalanceChanged, settings, time, blockchainUpdateTriggers, loadActiveLeases(db, _, _)),
      levelDBWriter
    )
  }
}
