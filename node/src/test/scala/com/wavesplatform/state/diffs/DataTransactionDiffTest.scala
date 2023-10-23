package com.wavesplatform.state.diffs

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock.{BlockWithSigner, create as block}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.RideV3
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}

class DataTransactionDiffTest extends PropSpec with WithDomain {

  val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.DataTransaction.id -> 0))

  val baseSetup: (GenesisTransaction, KeyPair) = {
    val master = TxHelpers.signer(1)

    val genesis = TxHelpers.genesis(master.toAddress)

    (genesis, master)
  }

  property("state invariants hold") {
    val setup = {
      val (genesis, master) = baseSetup

      val key1    = "key1"
      val item1   = IntegerDataEntry(key1, 1)
      val dataTx1 = TxHelpers.data(master, List(item1))

      val key2    = "key2"
      val item2   = BooleanDataEntry(key2, true)
      val dataTx2 = TxHelpers.data(master, List(item2))

      val item3   = IntegerDataEntry(key1, 3)
      val dataTx3 = TxHelpers.data(master, List(item3))

      (genesis, Seq(item1, item2, item3), Seq(dataTx1, dataTx2, dataTx3))
    }

    val (genesisTx, items, txs) = setup
    val sender                  = txs.head.sender.toAddress

    val item1 = items.head
    withDomain(RideV3) { d =>
      d.appendBlock(genesisTx)
      d.appendBlock(txs(0))
      d.liquidSnapshot.balances((sender, Waves)) shouldBe (ENOUGH_AMT - txs(0).fee.value)
      d.liquidSnapshot.accountData(sender)(item1.key) shouldBe item1
      assertBalanceInvariant(d.liquidSnapshot, d.rocksDBWriter, -txs(0).fee.value * 3 / 5)
    }

    val item2 = items(1)
    withDomain(RideV3) { d =>
      d.appendBlock(genesisTx)
      d.appendBlock(txs(0), txs(1))
      d.liquidSnapshot.balances((sender, Waves)) shouldBe (ENOUGH_AMT - txs.take(2).map(_.fee.value).sum)
      d.liquidSnapshot.accountData(sender)(item1.key) shouldBe item1
      d.liquidSnapshot.accountData(sender)(item2.key) shouldBe item2
      assertBalanceInvariant(d.liquidSnapshot, d.rocksDBWriter, -(txs(0).fee.value + txs(1).fee.value) * 3 / 5)
    }

    val item3 = items(2)
    withDomain(RideV3) { d =>
      d.appendBlock(genesisTx)
      d.appendBlock(txs(0), txs(1), txs(2))
      d.liquidSnapshot.balances((sender, Waves)) shouldBe (ENOUGH_AMT - txs.map(_.fee.value).sum)
      d.liquidSnapshot.accountData(sender)(item1.key) shouldBe item3
      d.liquidSnapshot.accountData(sender)(item2.key) shouldBe item2
      assertBalanceInvariant(d.liquidSnapshot, d.rocksDBWriter,  -(txs(0).fee.value + txs(1).fee.value + txs(2).fee.value) * 3 / 5)
    }
  }

  property("cannot overspend funds") {
    val setup = {
      val (genesis, master) = baseSetup
      val dataTx            = TxHelpers.data(master, List(BinaryDataEntry("key", ByteStr.fill(64)(1))), fee = ENOUGH_AMT + 1)

      (genesis, dataTx)
    }

    val (genesis, dataTx) = setup
    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(dataTx)), fs) { blockDiffEi =>
      blockDiffEi should produce("negative waves balance")
    }
  }

  property("validation fails prior to feature activation") {
    val setup = {
      val (genesis, master) = baseSetup
      val dataTx            = TxHelpers.data(master, List())

      (genesis, dataTx)
    }
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.DataTransaction.id -> 10))

    val (genesis, data) = setup
    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(data)), settings) { blockDiffEi =>
      blockDiffEi should produce("Data Transaction feature has not been activated")
    }
  }
}
