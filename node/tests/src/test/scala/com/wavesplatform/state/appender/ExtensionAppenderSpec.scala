package com.wavesplatform.state.appender

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.network.{ExtensionBlocks, InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.SystemTime
import com.wavesplatform.utx.UtxPoolImpl
import io.netty.channel.embedded.EmbeddedChannel
import monix.execution.Scheduler.Implicits.global

class ExtensionAppenderSpec extends FlatSpec with WithDomain {
  "Extension appender" should "drop duplicate transactions from UTX" in
    withDomain(balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val utx  = new UtxPoolImpl(SystemTime, d.blockchain, d.settings.utxSettings, d.settings.maxTxErrorLogSize, d.settings.minerSettings.enable)
      val time = TestTime()
      val extensionAppender = ExtensionAppender(d.blockchain, utx, d.posSelector, time, InvalidBlockStorage.NoOp, PeerDatabase.NoOp, global)(null, _)

      val tx     = TxHelpers.transfer()
      val block1 = d.createBlock(Seq(tx), strictTime = true, version = Block.PlainBlockVersion)
      utx.putIfNew(tx).resultE.explicitGet()
      d.appendBlock(tx)
      utx.all shouldBe Seq(tx)

      time.setTime(block1.header.timestamp)
      extensionAppender(ExtensionBlocks(d.blockchain.score + block1.blockScore(), Seq(block1), Map.empty, new EmbeddedChannel()))
        .runSyncUnsafe()
        .explicitGet()
      d.blockchain.height shouldBe 2
      utx.all shouldBe Nil
      utx.close()
    }

  it should "reject a fork deeper than max-rollback while finality lags further behind" in {
    val settings = DomainPresets.DeterministicFinality.copy(synchronizationSettings =
      DomainPresets.DeterministicFinality.synchronizationSettings.copy(maxRollback = 1)
    )
    withDomain(settings, AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val utx = new UtxPoolImpl(SystemTime, d.blockchain, d.settings.utxSettings, d.settings.maxTxErrorLogSize, d.settings.minerSettings.enable)
      val extensionAppender =
        ExtensionAppender(d.blockchain, utx, d.posSelector, d.testTime, InvalidBlockStorage.NoOp, PeerDatabase.NoOp, global)(new EmbeddedChannel(), _)

      d.appendBlock()
      val fork3 = d.createBlock(strictTime = true)
      d.appendBlock()
      d.appendBlock()
      d.blockchain.finalizedHeight.fold(0)(_.toInt) should be <= 2 // finality alone would let a fork from height 2 through
      d.blockchain.syncBoundary shouldBe Height(3)
      val tip = d.blockchain.lastBlockId

      extensionAppender(ExtensionBlocks(d.blockchain.score + 1, Seq(fork3), Map.empty, new EmbeddedChannel()))
        .runSyncUnsafe()
        .left
        .value
        .toString should include("below the sync boundary 3")
      d.blockchain.height shouldBe 4
      d.blockchain.lastBlockId shouldBe tip
      utx.close()
    }
  }
}
