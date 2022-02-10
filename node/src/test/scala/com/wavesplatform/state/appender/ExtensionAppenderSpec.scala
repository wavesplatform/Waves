package com.wavesplatform.state.appender

import com.wavesplatform.TestTime
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.network.{ExtensionBlocks, InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.SystemTime
import com.wavesplatform.utx.UtxPoolImpl
import monix.execution.Scheduler.Implicits.global

class ExtensionAppenderSpec extends FlatSpec with WithDomain {
  "Extension appender" should "drop duplicate transactions from UTX" in
    withDomain(balances = Seq(AddrWithBalance(TxHelpers.defaultAddress))) { d =>
      val utx               = new UtxPoolImpl(SystemTime, d.blockchain, d.settings.utxSettings)
      val time              = new TestTime()
      val extensionAppender = ExtensionAppender(d.blockchain, utx, d.posSelector, time, InvalidBlockStorage.NoOp, PeerDatabase.NoOp, global)(null, _)

      val tx     = TxHelpers.transfer()
      val block1 = d.createBlock(Block.PlainBlockVersion, Seq(tx), strictTime = true)
      utx.putIfNew(tx).resultE.explicitGet()
      d.appendBlock(tx)
      utx.all shouldBe Seq(tx)

      time.setTime(block1.header.timestamp)
      extensionAppender(ExtensionBlocks(d.blockchain.score + block1.blockScore(), Seq(block1))).runSyncUnsafe().explicitGet()
      d.blockchain.height shouldBe 2
      utx.all shouldBe Nil
    }

}
