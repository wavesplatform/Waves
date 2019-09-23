package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.settings.{BlockchainSettings, DBSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import com.wavesplatform.transaction.{Asset, Transaction, TransactionParsers}
import monix.reactive.Observer
import org.iq80.leveldb.DB

import scala.concurrent.duration.Duration

package object utils {
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

  implicit class BlockchainAddressTransactionsList(b: Blockchain) {
    def addressTransactions(address: Address,
                            types: Set[Transaction.Type],
                            count: Int,
                            fromId: Option[ByteStr]): Either[String, Seq[(Height, Transaction)]] = {
      import monix.execution.Scheduler.Implicits.global

      def createTransactionsList(): Seq[(Height, Transaction)] =
        b.addressTransactionsObservable(address, TransactionParsers.forTypeSet(types), fromId)
          .take(count)
          .toListL
          .runSyncUnsafe(Duration.Inf)

      fromId match {
        case Some(id) => b.transactionInfo(id).toRight(s"Transaction $id does not exist").map(_ => createTransactionsList())
        case None     => Right(createTransactionsList())
      }
    }
  }
}
