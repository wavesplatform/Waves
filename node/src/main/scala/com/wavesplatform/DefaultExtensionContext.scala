package com.wavesplatform

import akka.actor.ActorSystem
import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonBlocksApi, CommonTransactionsApi}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.{BlockchainUpdated, UtxEvent}
import com.wavesplatform.extensions.Context
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, Diff, Height}
import com.wavesplatform.transaction.{Asset, DiscardedBlocks, Transaction}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.reactive.Observable
import org.iq80.leveldb.DB

class DefaultExtensionContext(
    blockchainUpdater: BlockchainUpdaterImpl,
    db: DB,
    val settings: WavesSettings,
    val time: Time,
    val wallet: Wallet,
    val actorSystem: ActorSystem,
    val spendableBalanceChanged: Observable[(Address, Asset)],
    val utxEvents: Observable[UtxEvent],
    val utx: UtxPool,
    val blockchainUpdated: Observable[BlockchainUpdated],
    rollbackTask: ByteStr => Task[Either[ValidationError, DiscardedBlocks]],
    publishTransaction: Transaction => TracedResult[ValidationError, Boolean],
    metaAt: Int => Option[BlockMeta],
    blockAt: Int => Option[(BlockMeta, Seq[Transaction])]
) extends Context {
  override def blockchain: Blockchain                                                        = blockchainUpdater.blockchain
  override def rollbackTo(blockId: ByteStr): Task[Either[ValidationError, DiscardedBlocks]]  = rollbackTask(blockId)
  override def broadcastTransaction(tx: Transaction): TracedResult[ValidationError, Boolean] = publishTransaction(tx)

  override val transactionsApi: CommonTransactionsApi = CommonTransactionsApi(
    blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.blockchain.height) -> diff),
    db,
    blockchainUpdater.blockchain,
    utx,
    wallet,
    publishTransaction,
    blockAt
  )
  override val blocksApi: CommonBlocksApi = CommonBlocksApi(blockchainUpdater.blockchain, metaAt, blockAt)
  override val accountsApi: CommonAccountsApi =
    CommonAccountsApi(blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), db, blockchainUpdater.blockchain)
  override val assetsApi: CommonAssetsApi =
    CommonAssetsApi(blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), db, blockchainUpdater.blockchain)
}
