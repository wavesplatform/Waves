package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{AddressTransactions, UseLiquidDiff}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{LevelDBWriter, TestStorageFactory}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.settings.TestSettings.*
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings, TestSettings}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.SystemTime
import monix.reactive.Observer
import org.iq80.leveldb.DB

package object utils {

  def addressTransactions(
      db: DB,
      diff: => Option[(Height, Diff)],
      address: Address,
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Seq[(Height, Transaction)] = {
    val useDiff = new UseLiquidDiff { override def apply[A](f: Option[(Height, Diff)] => A): A = f(diff) }
    AddressTransactions.allAddressTransactions(db, useDiff, address, None, types, fromId).map { case (tm, tx) => tm.height -> tx }.toSeq
  }

  object TestLevelDB {
    def withFunctionalitySettings(
        writableDB: DB,
        spendableBalanceChanged: Observer[(Address, Asset)],
        fs: FunctionalitySettings
    ): LevelDBWriter =
      TestStorageFactory(
        TestSettings.Default.withFunctionalitySettings(fs),
        writableDB,
        SystemTime,
        spendableBalanceChanged,
        BlockchainUpdateTriggers.noop
      )._2

    def createTestBlockchainSettings(fs: FunctionalitySettings): BlockchainSettings =
      BlockchainSettings('T', fs, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
  }
}
