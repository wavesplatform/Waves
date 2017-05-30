package com.wavesplatform.mining

import java.util.concurrent.{Executors, TimeUnit}

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings}
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.transaction.{History, PoSCalc, UnconfirmedTransactionsStorage}
import scorex.utils.Time

class Miner(
    history: History,
    state: StateReader,
    utx: UnconfirmedTransactionsStorage,
    privateKeyAccounts: => Seq[PrivateKeyAccount],
    time: Time,
    fs: FunctionalitySettings,
    bcs: BlockchainSettings) {
  private val minerPool = Executors.newScheduledThreadPool(4)

  private def mkTask(height: Int, parent: Block, greatGrandParent: Option[Block], account: PrivateKeyAccount) =
    new ForgeBlockTask(height, parent, greatGrandParent, state, utx, time, bcs, account)

  def lastBlockChanged(height: Int, lastBlock: Block): Unit = {
    val greatGrandParent = history.blockAt(height - 3)
    for (pka <- privateKeyAccounts; ts <- PoSCalc.nextBlockGenerationTime(height, state, fs, lastBlock, pka)) {
      minerPool.schedule(mkTask(height, lastBlock, greatGrandParent, pka), ts - time.correctedTime(), TimeUnit.MILLISECONDS)

    }

  }

  def shutdown(): Unit = {
    minerPool.shutdownNow()
  }
}
