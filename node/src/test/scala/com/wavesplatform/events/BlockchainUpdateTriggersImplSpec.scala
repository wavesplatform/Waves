package com.wavesplatform.events

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.diffs.{BlockDiffer, ENOUGH_AMT}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.{NoShrink, RequestGen}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ReplaySubject
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class BlockchainUpdateTriggersImplSpec extends FreeSpec with Matchers with RequestGen with ScalaCheckPropertyChecks with NoShrink with WithDomain {

  private val sigGen: Gen[ByteStr] = bytes64gen.map(ByteStr.apply)
  private val heightGen: Gen[Int]  = Gen.choose(1, 1000)

  private val FEE = 1000000

  private def produceEvent(useTrigger: BlockchainUpdateTriggers => Unit): BlockchainUpdated = {
    val evts = ReplaySubject[BlockchainUpdated]()
    val t    = new BlockchainUpdateTriggersImpl(evts)
    useTrigger(t)
    evts.onComplete()
    evts.toListL.runSyncUnsafe(500.milliseconds).head
  }

  private def withBlockchainAndGenesis[A](test: (Blockchain, KeyPair) => A): A =
    withDomain() { d =>
      val masterAccount = accountGen.sample.get
      val genesisBlock = TestBlock
        .create(0, Seq(GenesisTransaction.create(masterAccount, ENOUGH_AMT, 0).explicitGet()))
      d.blockchainUpdater.processBlock(genesisBlock).explicitGet()
      test(d.blockchainUpdater, masterAccount)
    }

  private def detailedDiffFromBlock(b: Block, bc: Blockchain): DetailedDiff =
    BlockDiffer.fromBlock(bc, None, b, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff

  private def detailedDiffFromMicroBlock(mb: MicroBlock, bc: Blockchain): DetailedDiff =
    BlockDiffer.fromMicroBlock(bc, Some(0), mb, 1, MiningConstraint.Unlimited, verify = false).explicitGet().detailedDiff

  private def areBlocksEqual(b1: Block, b2: Block): Boolean =
    b1.signature == b2.signature && b1.transactionData == b2.transactionData

  "rollbacks" - {
    "block rollback produces correct events in the observer" in forAll(sigGen, heightGen) { (sig, height) =>
      produceEvent(_.onRollback(sig, height)) match {
        case RollbackCompleted(toId, toHeight) =>
          toId shouldBe sig
          toHeight shouldBe height
        case _ => fail()
      }
    }

    "microblock rollback produces correct events in the observer" in forAll(sigGen, heightGen) { (sig, height) =>
      produceEvent(_.onMicroBlockRollback(sig, height)) match {
        case MicroBlockRollbackCompleted(toId, toHeight) =>
          toId shouldBe sig
          toHeight shouldBe height
        case _ => fail()
      }
    }
  }

  "appends" - {
    "empty block" in withBlockchainAndGenesis { (bc, _) =>
      val b = TestBlock.create(1, Seq.empty)
      produceEvent(_.onProcessBlock(b, detailedDiffFromBlock(b, bc), bc)) match {
        case BlockAppended(toId, toHeight, block, blockStateUpdate, transactionStateUpdates) =>
          toId shouldBe b.signature
          toHeight shouldBe bc.height + 1
          areBlocksEqual(b, block) shouldBe true

          blockStateUpdate.isEmpty shouldBe true
          transactionStateUpdates.isEmpty shouldBe true
        case _ => fail()
      }
    }

    "block with balance updates" in withBlockchainAndGenesis { (bc, master) =>
      val recipient = accountGen.sample.get.publicKey.toAddress
      val miner     = accountGen.sample.get

      val tx = TransferTransaction
        .selfSigned(1.toByte, master, recipient, Waves, ENOUGH_AMT / 5, Waves, FEE, None, 1)
        .explicitGet()

      val b = TestBlock.create(miner, Seq(tx))

      produceEvent(_.onProcessBlock(b, detailedDiffFromBlock(b, bc), bc)) match {
        case BlockAppended(toId, toHeight, block, blockStateUpdate, transactionStateUpdates) =>
          toId shouldBe b.signature
          toHeight shouldBe bc.height + 1
          areBlocksEqual(b, block) shouldBe true

          println(blockStateUpdate)
          println(transactionStateUpdates)

          // miner reward
          blockStateUpdate.balances.head match {
            case (address, asset, newBalance) =>
              address shouldBe miner.publicKey.toAddress
              asset shouldBe Waves
              newBalance shouldBe FEE
          }

          val masterUpd = transactionStateUpdates.head.balances.find(_._1 == master.publicKey.toAddress).get
          masterUpd._3 shouldBe (ENOUGH_AMT - tx.amount - FEE)

          val recipientUpd = transactionStateUpdates.head.balances.find(_._1 == recipient).get
          recipientUpd._3 shouldBe tx.amount

        case _ => fail()
      }
    }

    "block with data entries" in withBlockchainAndGenesis { (bc, master) =>
      val tx = dataTransactionGen.sample.get

      val b = TestBlock.create(master, Seq(tx))

      produceEvent(_.onProcessBlock(b, detailedDiffFromBlock(b, bc), bc)) match {
        case BlockAppended(toId, toHeight, block, blockStateUpdate, transactionStateUpdates) =>
          toId shouldBe b.signature
          toHeight shouldBe bc.height + 1
          areBlocksEqual(b, block) shouldBe true
          transactionStateUpdates.head.dataEntries.map(_._2).sortBy(_.key) shouldBe tx.data.sortBy(_.key)
        case _ => fail()
      }
    }

    // @todo different assets state update
  }
}
