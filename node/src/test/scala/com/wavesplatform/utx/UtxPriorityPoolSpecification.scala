package com.wavesplatform.utx

import com.wavesplatform.db.WithState
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utx.UtxPool.PackStrategy

class UtxPriorityPoolSpecification extends FreeSpec with SharedDomain {
  private val alice = TxHelpers.signer(100)

  private var lastKeyPair = 0
  private def nextKeyPair = {
    lastKeyPair += 1
    TxHelpers.signer(lastKeyPair)
  }

  override val genesisBalances: Seq[WithState.AddrWithBalance] = Seq(alice -> 10000.waves)

  override def settings: WavesSettings = DomainPresets.RideV3

  private def pack() = domain.utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1

  "priority pool" - {
    "preserves correct order of transactions" in {
      val id = domain.appendKeyBlock().id()
      val t1 = TxHelpers.transfer(alice, nextKeyPair.toAddress, fee = 0.001.waves)
      val t2 = TxHelpers.transfer(alice, nextKeyPair.toAddress, fee = 0.01.waves, timestamp = t1.timestamp - 10000)

      domain.appendMicroBlock(t1)
      domain.appendMicroBlock(t2)
      domain.appendKeyBlock(Some(id))

      val expectedTransactions = Seq(t1, t2)
      domain.utxPool.priorityPool.priorityTransactions shouldBe expectedTransactions
      pack() shouldBe Some(expectedTransactions)
    }

    "takes into account priority txs when packing" in {
      val id        = domain.appendKeyBlock().id()
      val bob       = nextKeyPair
      val transfer1 = TxHelpers.transfer(alice, bob.toAddress, 10.001.waves, fee = 0.001.waves)
      domain.appendMicroBlock(transfer1)
      domain.appendKeyBlock(Some(id))

      domain.utxPool.priorityPool.priorityTransactions shouldBe Seq(transfer1)

      val transfer2 = TxHelpers.transfer(bob, nextKeyPair.toAddress, 10.waves, fee = 0.001.waves)
      domain.utxPool.putIfNew(transfer2).resultE should beRight
      domain.utxPool.nonPriorityTransactions shouldBe Seq(transfer2)
      pack() shouldBe Some(Seq(transfer1, transfer2))
    }

    "counts microblock size from priority diffs" in {
      val ref = domain.appendKeyBlock().id()
      domain.appendMicroBlock(Seq.tabulate(5) { i =>
        TxHelpers.transfer(alice, TxHelpers.signer(200 + i).toAddress)
      }: _*)
      domain.appendMicroBlock(Seq.tabulate(5) { i =>
        TxHelpers.transfer(alice, TxHelpers.signer(300 + i).toAddress)
      }: _*)

      domain.appendKeyBlock(Some(ref))
      // priority pool contains two microblocks, 5 txs each
      domain.utxPool.priorityPool.nextMicroBlockSize(3) shouldBe 5
      domain.utxPool.priorityPool.nextMicroBlockSize(5) shouldBe 5
      domain.utxPool.priorityPool.nextMicroBlockSize(8) shouldBe 5
      domain.utxPool.priorityPool.nextMicroBlockSize(10) shouldBe 10
      domain.utxPool.priorityPool.nextMicroBlockSize(12) shouldBe 12
    }

    "doesn't run cleanup on priority pool" in {}

    "invalidates priority pool on different microblock" in {}

    "continues packing when priority diff contains no valid transactions" in {
      val bob = nextKeyPair
      domain.appendBlock(
        TxHelpers.transfer(alice, bob.toAddress, 10.015.waves, fee = 0.001.waves),
        TxHelpers.setScript(bob, TestCompiler(V3).compileExpression(s"height % 2 == ${domain.blockchain.height % 2}"), fee = 0.01.waves)
      )
      val ref = domain.appendKeyBlock().id()
      val transfer1 = TxHelpers.transfer(bob, nextKeyPair.toAddress, 10.waves, fee = 0.005.waves)
      domain.appendMicroBlock(transfer1)
      domain.appendKeyBlock(Some(ref))
      domain.utxPool.priorityPool.priorityTransactions shouldEqual Seq(transfer1)

      val transfer2 = TxHelpers.transfer(alice, nextKeyPair.toAddress, fee = 0.001.waves)
      domain.utxPool.putIfNew(transfer2).resultE should beRight
      domain.utxPool.all shouldEqual Seq(transfer1, transfer2)

      pack() shouldEqual Some(Seq(transfer2))
    }
  }
}
