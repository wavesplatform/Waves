package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.history.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}
import com.wavesplatform.transaction.transfer.*

class NgStateTest extends PropSpec {

  def preconditionsAndPayments(amt: Int): (GenesisTransaction, Seq[TransferTransaction]) = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis  = TxHelpers.genesis(master.toAddress)
    val payments = (1 to amt).map(idx => TxHelpers.transfer(master, recipient.toAddress, idx))

    (genesis, payments)
  }

  property("can forge correctly signed blocks") {
    val (genesis, payments)  = preconditionsAndPayments(10)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = NgState(block, StateSnapshot.empty, 0L, 0L, ByteStr.empty, Set.empty, None, block.header.generationSignature, Map.empty)
    microBlocks.foreach(m => ng = ng.append(m, StateSnapshot.empty, 0L, 0L, 0L, ByteStr.empty))

    ng.snapshotOf(microBlocks.last.totalResBlockSig)
    microBlocks.foreach { m =>
      val (forged, _, _, _, _, _) = ng.snapshotOf(m.totalResBlockSig).get
      forged.signatureValid() shouldBe true
    }
    Seq(microBlocks(4)).map(x => ng.snapshotOf(x.totalResBlockSig))
  }

  property("can resolve best liquid block") {
    val (genesis, payments)  = preconditionsAndPayments(5)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = NgState(block, StateSnapshot.empty, 0L, 0L, ByteStr.empty, Set.empty, None, block.header.generationSignature, Map.empty)
    microBlocks.foreach(m => ng = ng.append(m, StateSnapshot.empty, 0L, 0L, 0L, ByteStr.empty))

    ng.bestLiquidBlock.id() shouldBe microBlocks.last.totalResBlockSig

    new NgState(block, StateSnapshot.empty, 0L, 0L, ByteStr.empty, Set.empty, Some(0), block.header.generationSignature, Map.empty).bestLiquidBlock
      .id() shouldBe block
      .id()
  }

  property("can resolve best last block") {
    val (genesis, payments)  = preconditionsAndPayments(5)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = NgState(block, StateSnapshot.empty, 0L, 0L, ByteStr.empty, Set.empty, None, block.header.generationSignature, Map.empty)

    microBlocks.foldLeft(1000) { case (thisTime, m) =>
      ng = ng.append(m, StateSnapshot.empty, 0L, 0L, thisTime, ByteStr.empty)
      thisTime + 50
    }

    ng.bestLastBlockInfo(0).blockId shouldBe block.id()
    ng.bestLastBlockInfo(1001).blockId shouldBe microBlocks.head.totalResBlockSig
    ng.bestLastBlockInfo(1051).blockId shouldBe microBlocks.tail.head.totalResBlockSig
    ng.bestLastBlockInfo(2000).blockId shouldBe microBlocks.last.totalResBlockSig

    new NgState(block, StateSnapshot.empty, 0L, 0L, ByteStr.empty, Set.empty, Some(0), block.header.generationSignature, Map.empty).bestLiquidBlock
      .id() shouldBe block
      .id()
  }

  property("calculates carry fee correctly") {
    val (genesis, payments)  = preconditionsAndPayments(5)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = NgState(block, StateSnapshot.empty, 0L, 0L, ByteStr.empty, Set.empty, None, block.header.generationSignature, Map.empty)
    microBlocks.foreach(m => ng = ng.append(m, StateSnapshot.empty, 1L, 0L, 0L, ByteStr.empty))

    ng.snapshotOf(block.id()).map(_._3) shouldBe Some(0L)
    microBlocks.zipWithIndex.foreach { case (m, i) =>
      val u = ng.snapshotOf(m.totalResBlockSig).map(_._3)
      u shouldBe Some(i + 1)
    }
    ng.carryFee shouldBe microBlocks.size
  }
}
