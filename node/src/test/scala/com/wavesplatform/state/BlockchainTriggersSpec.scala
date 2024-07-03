package com.wavesplatform.state

import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.block.{Block, MicroBlock, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.OptionValues

class BlockchainTriggersSpec extends PropSpec with WithDomain with OptionValues {
  private val richAccount = TxHelpers.signer(2000)

  property("proper events") {
    withDomain(DomainPresets.TransactionStateSnapshot, Seq(AddrWithBalance(richAccount.toAddress, 10_000.waves))) { d =>
      val rollbackTo = d.appendBlock(TxHelpers.dataEntry(richAccount, IntegerDataEntry("foo_key", 1))).id()
      d.appendMicroBlock(TxHelpers.dataEntry(richAccount, IntegerDataEntry("foo_key", 2)))

      d.blockchain.accountData(richAccount.toAddress, "foo_key").value shouldEqual IntegerDataEntry("foo_key", 2)

      d.triggers :+= new BlockchainUpdateTriggers {
        override def onProcessBlock(
            block: Block,
            snapshot: StateSnapshot,
            reward: Option[Long],
            hitSource: ByteStr,
            blockchainBeforeWithReward: Blockchain
        ): Unit = {
          blockchainBeforeWithReward.accountData(richAccount.toAddress, "foo_key").value.cast[IntegerDataEntry].value.value shouldEqual 1
          val updated =
            SnapshotBlockchain(blockchainBeforeWithReward, Some(snapshot), Some(SignedBlockHeader(block.header, block.signature) -> hitSource))
          updated.accountData(richAccount.toAddress, "foo_key").value.cast[IntegerDataEntry].value.value shouldEqual 1
        }

        override def onProcessMicroBlock(
            microBlock: MicroBlock,
            snapshot: StateSnapshot,
            blockchainBeforeWithReward: Blockchain,
            totalBlockId: ByteStr,
            totalTransactionsRoot: ByteStr
        ): Unit = {
          val updated = SnapshotBlockchain(
            blockchainBeforeWithReward,
            Some(snapshot.transactions.values.foldLeft(StateSnapshot.empty) { (s, n2) =>
              s |+| n2.snapshot
            })
          )
          updated.accountData(richAccount.toAddress, "foo_key").value.cast[IntegerDataEntry].value.value shouldEqual 5
        }

        override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit = {}

        override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit = {}
      }
      d.appendKeyBlock(ref = Some(rollbackTo))
      d.appendMicroBlock(TxHelpers.dataEntry(richAccount, IntegerDataEntry("foo_key", 5)))
    }
  }
}
