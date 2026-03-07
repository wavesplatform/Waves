package com.wavesplatform.state

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.history.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}

class NgStateTest extends PropSpec {
  private def preconditionsAndPayments(amt: Int): (GenesisTransaction, Seq[TransferTransaction]) = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis  = TxHelpers.genesis(master.toAddress)
    val payments = (1 to amt).map(idx => TxHelpers.transfer(master, recipient.toAddress, idx))

    (genesis, payments)
  }

  private def mkNgState(block: Block): NgState = NgState(
    block,
    StateSnapshot.empty,
    baseBlockCarry = 0L,
    baseBlockTotalFee = 0L,
    baseBlockComputedStateHash = ByteStr.empty,
    approvedFeatures = Set.empty,
    reward = None,
    hitSource = block.header.generationSignature,
    leasesToCancel = Map.empty,
    FinalizationState.notActivated(block)
  )

  property("can forge correctly signed blocks") {
    val (genesis, payments)  = preconditionsAndPayments(10)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = mkNgState(block)
    microBlocks.foreach(m => ng = ng.append(m, StateSnapshot.empty, 0L, 0L, 0L, ByteStr.empty, None, Seq.empty))

    ng.liquidBlockOf(microBlocks.last.totalResBlockSig)
    microBlocks.foreach { m =>
      val r = ng.liquidBlockOf(m.totalResBlockSig).get
      r.block.signatureValid() shouldBe true
    }
    Seq(microBlocks(4)).foreach { x =>
      ng.liquidBlockOf(x.totalResBlockSig) shouldBe defined
    }
  }

  property("can resolve best liquid block") {
    val (genesis, payments)  = preconditionsAndPayments(5)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = mkNgState(block)
    microBlocks.foreach(m => ng = ng.append(m, StateSnapshot.empty, 0L, 0L, 0L, ByteStr.empty, None, Seq.empty))

    ng.bestLiquidBlock.id() shouldBe microBlocks.last.totalResBlockSig
    mkNgState(block).bestLiquidBlock.id() shouldBe block.id()
  }

  property("can resolve best last block") {
    val (genesis, payments)  = preconditionsAndPayments(5)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = mkNgState(block)

    microBlocks.foldLeft(1000) { case (thisTime, m) =>
      ng = ng.append(m, StateSnapshot.empty, 0L, 0L, thisTime, ByteStr.empty, None, Seq.empty)
      thisTime + 50
    }

    ng.bestLastBlockInfo(0).blockId shouldBe block.id()
    ng.bestLastBlockInfo(1001).blockId shouldBe microBlocks.head.totalResBlockSig
    ng.bestLastBlockInfo(1051).blockId shouldBe microBlocks.tail.head.totalResBlockSig
    ng.bestLastBlockInfo(2000).blockId shouldBe microBlocks.last.totalResBlockSig

    mkNgState(block).bestLiquidBlock.id() shouldBe block.id()
  }

  property("calculates carry fee correctly") {
    val (genesis, payments)  = preconditionsAndPayments(5)
    val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

    var ng = mkNgState(block)
    microBlocks.foreach(m => ng = ng.append(m, StateSnapshot.empty, 1L, 0L, 0L, ByteStr.empty, None, Seq.empty))

    ng.liquidBlockOf(block.id()).map(_.data.carryFee) shouldBe Some(0L)
    microBlocks.zipWithIndex.foreach { case (m, i) =>
      val u = ng.liquidBlockOf(m.totalResBlockSig).map(_.data.carryFee)
      u shouldBe Some(i + 1)
    }
    ng.carryFee shouldBe microBlocks.size
  }
}
