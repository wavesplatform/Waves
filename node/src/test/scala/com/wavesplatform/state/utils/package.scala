package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.AddressTransactions
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBResource, LevelDBWriter}
import com.wavesplatform.settings.{BlockchainSettings, DBSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Asset, Transaction}
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
    val resource = DBResource(db)
    try AddressTransactions.allAddressTransactions(resource, diff, address, None, types, fromId).toSeq
    finally resource.close()
  }

  def nftList(address: Address): Seq[IssueTransaction] = ???

  object TestLevelDB {
    def withFunctionalitySettings(
        writableDB: DB,
        spendableBalanceChanged: Observer[(Address, Asset)],
        fs: FunctionalitySettings,
        dbSettings: DBSettings
    ): LevelDBWriter =
      new LevelDBWriter(
        writableDB,
        spendableBalanceChanged,
        createTestBlockchainSettings(fs),
        dbSettings
      )

    def createTestBlockchainSettings(fs: FunctionalitySettings): BlockchainSettings =
      BlockchainSettings('T', fs, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
  }
}
