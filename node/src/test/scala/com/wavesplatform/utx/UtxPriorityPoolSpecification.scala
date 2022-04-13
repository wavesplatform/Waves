package com.wavesplatform.utx

import com.wavesplatform.db.WithState
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utx.UtxPool.PackStrategy

class UtxPriorityPoolSpecification extends FreeSpec with SharedDomain {
  private val alice = TxHelpers.signer(100)
  private val bob   = TxHelpers.signer(101)
  private val carol = TxHelpers.signer(103)

  override val genesisBalances: Seq[WithState.AddrWithBalance] = Seq(alice -> 100.waves)

  "priority pool" - {
    "preserves correct order of transactions" in {}

    "takes into account priority txs when pack" in {
      val id        = domain.appendKeyBlock().id()
      val transfer1 = TxHelpers.transfer(alice, bob.toAddress, 10.waves)
      domain.appendMicroBlock(transfer1)
      domain.appendKeyBlock(Some(id))

      domain.utxPool.priorityPool.priorityTransactions shouldBe Seq(transfer1)

      val transfer2 = TxHelpers.transfer(bob, carol.toAddress, 10.waves)
      domain.utxPool.putIfNew(transfer2).resultE should beRight
      domain.utxPool.nonPriorityTransactions shouldBe Seq(transfer2)
      domain.utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(Seq(transfer1, transfer2))
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

    "doesn't run cleanup on priority pool" in {
    }

    "invalidates priority pool on different microblock" in {
    }

    "continues packing when priority diff contains no valid transactions" in {
    }
  }
}
