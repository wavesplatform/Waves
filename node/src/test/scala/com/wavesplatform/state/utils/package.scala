package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.AddressTransactions
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{LevelDBWriter, TestStorageFactory}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings, TestSettings}
import com.wavesplatform.settings.TestSettings._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.SystemTime
import org.iq80.leveldb.DB

package object utils {

  def addressTransactions(
      db: DB,
      diff: => Option[(Height, Diff)],
      address: Address,
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Seq[(Height, Transaction)] =
    AddressTransactions.allAddressTransactions(db, diff, address, None, types, fromId).map { case (h, tx, _) => h -> tx }.toSeq

  object TestLevelDB {
    def withFunctionalitySettings(writableDB: DB, fs: FunctionalitySettings): LevelDBWriter =
      TestStorageFactory(TestSettings.Default.withFunctionalitySettings(fs), writableDB, SystemTime, BlockchainUpdateTriggers.noop)._2

    def createTestBlockchainSettings(fs: FunctionalitySettings): BlockchainSettings =
      BlockchainSettings('T', fs, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
  }
}
