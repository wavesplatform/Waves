package com.wavesplatform.state.appender

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.network.{ExtensionBlocks, InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.SystemTime
import com.wavesplatform.utx.UtxPoolImpl
import monix.execution.Scheduler.Implicits.global

class ExtensionAppenderSpec extends FlatSpec with WithDomain {
  "Extension appender" should "drop duplicate transactions from UTX" in
    withDomain(balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val utx  = new UtxPoolImpl(SystemTime, d.blockchain, d.settings.utxSettings, d.settings.maxTxErrorLogSize, d.settings.minerSettings.enable)
      val time = TestTime()
      val extensionAppender = ExtensionAppender(d.blockchain, utx, d.posSelector, time, InvalidBlockStorage.NoOp, PeerDatabase.NoOp, global)(null, _)

      val tx     = TxHelpers.transfer()
      val block1 = d.createBlock(Block.PlainBlockVersion, Seq(tx), strictTime = true)
      utx.putIfNew(tx).resultE.explicitGet()
      d.appendBlock(tx)
      utx.all shouldBe Seq(tx)

      time.setTime(block1.header.timestamp)
      extensionAppender(ExtensionBlocks(d.blockchain.score + block1.blockScore(), Seq(block1), Map.empty)).runSyncUnsafe().explicitGet()
      d.blockchain.height shouldBe 2
      utx.all shouldBe Nil
      utx.close()
    }

}
