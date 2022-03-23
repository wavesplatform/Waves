package com.wavesplatform.state.diffs

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock.create as block
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}

class DataTransactionDiffTest extends PropSpec with WithState {

  val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.DataTransaction.id -> 0))

  val baseSetup: (GenesisTransaction, KeyPair) = {
    val master = TxHelpers.signer(1)

    val genesis = TxHelpers.genesis(master.toAddress)

    (genesis, master)
  }

  property("state invariants hold") {
    val setup = {
      val (genesis, master) = baseSetup

      val key1 = "key1"
      val item1 = IntegerDataEntry(key1, 1)
      val dataTx1 = TxHelpers.data(master, List(item1))

      val key2 = "key2"
      val item2 = BooleanDataEntry(key2, true)
      val dataTx2 = TxHelpers.data(master, List(item2))

      val item3 = IntegerDataEntry(key1, 3)
      val dataTx3 = TxHelpers.data(master, List(item3))

      (genesis, Seq(item1, item2, item3), Seq(dataTx1, dataTx2, dataTx3))
    }

    val (genesisTx, items, txs) = setup
    val sender  = txs.head.sender
    val genesis = block(Seq(genesisTx))
    val blocks  = txs.map(tx => block(Seq(tx)))

    val item1 = items.head
    assertDiffAndState(Seq(genesis), blocks(0), fs) {
      case (totalDiff, state) =>
        assertBalanceInvariant(totalDiff)
        state.balance(sender.toAddress) shouldBe (ENOUGH_AMT - txs(0).fee)
        state.accountData(sender.toAddress, item1.key) shouldBe Some(item1)
    }

    val item2 = items(1)
    assertDiffAndState(Seq(genesis, blocks(0)), blocks(1), fs) {
      case (totalDiff, state) =>
        assertBalanceInvariant(totalDiff)
        state.balance(sender.toAddress) shouldBe (ENOUGH_AMT - txs.take(2).map(_.fee).sum)
        state.accountData(sender.toAddress, item1.key) shouldBe Some(item1)
        state.accountData(sender.toAddress, item2.key) shouldBe Some(item2)
    }

    val item3 = items(2)
    assertDiffAndState(Seq(genesis, blocks(0), blocks(1)), blocks(2), fs) {
      case (totalDiff, state) =>
        assertBalanceInvariant(totalDiff)
        state.balance(sender.toAddress) shouldBe (ENOUGH_AMT - txs.map(_.fee).sum)
        state.accountData(sender.toAddress, item1.key) shouldBe Some(item3)
        state.accountData(sender.toAddress, item2.key) shouldBe Some(item2)
    }
  }

  property("cannot overspend funds") {
    val setup = {
      val (genesis, master) = baseSetup
      val dataTx = TxHelpers.data(master, List(BinaryDataEntry("key", ByteStr.fill(64)(1))), fee = ENOUGH_AMT + 1)

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
      val dataTx = TxHelpers.data(master, List())

      (genesis, dataTx)
    }
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.DataTransaction.id -> 10))

    val (genesis, data) = setup
    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(data)), settings) { blockDiffEi =>
      blockDiffEi should produce("Data Transaction feature has not been activated")
    }
  }
}
