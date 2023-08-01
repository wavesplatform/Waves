package com.wavesplatform.state.snapshot

import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.utils.EthTxGenerator

class StateSnapshotStorageTest extends PropSpec with WithDomain {
  property("transaction snapshot storage") {
    withDomain(RideV6.configure(_.copy(minAssetInfoUpdateInterval = 2))) { d =>
      def checkSnapshot(tx: Transaction): Unit = {
        d.appendBlock(tx)
        d.appendBlock()
        d.rocksDBWriter.transactionSnapshot(tx.id()) shouldBe defined
      }
      checkSnapshot(genesis(defaultAddress))
      checkSnapshot(payment())
      checkSnapshot(transfer())
      val script  = TestCompiler(V6).compileExpression("true")
      val issueTx = issue(script = Some(script))
      val asset   = IssuedAsset(issueTx.id())
      checkSnapshot(issueTx)
      checkSnapshot(reissue(asset))
      checkSnapshot(burn(asset))
      checkSnapshot(exchange(orderV3(BUY, asset), orderV3(SELL, asset)))
      val leaseTx = lease()
      checkSnapshot(leaseTx)
      checkSnapshot(leaseCancel(leaseTx.id()))
      checkSnapshot(createAlias("alias"))
      checkSnapshot(massTransfer(fee = 200_000))
      checkSnapshot(dataSingle())
      checkSnapshot(setScript(defaultSigner, script))
      val issue2 = issue()
      val asset2 = IssuedAsset(issue2.id())
      d.appendBlock(issue2)
      checkSnapshot(sponsor(asset2))
      checkSnapshot(setAssetScript(defaultSigner, asset, script))
      val setDApp = setScript(
        defaultSigner,
        TestCompiler(V6).compileContract(
          """
            | @Callable(i)
            | func default() = []
          """.stripMargin
        )
      )
      d.appendBlock(setDApp)
      checkSnapshot(invoke(dApp = defaultAddress))
      checkSnapshot(updateAssetInfo(asset2.id))
      val ethTransfer = EthTxGenerator.generateEthTransfer(defaultEthSigner, defaultAddress, 1, asset2)
      d.appendBlock(transfer(to = ethTransfer.senderAddress()), transfer(to = ethTransfer.senderAddress(), asset = asset2))
      checkSnapshot(ethTransfer)
    }
  }
}
