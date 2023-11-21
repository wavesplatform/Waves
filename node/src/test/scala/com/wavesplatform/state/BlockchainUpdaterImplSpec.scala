package com.wavesplatform.state

import com.google.common.primitives.Longs
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.history.{chainBaseAndMicro, randomSig}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.BlockAppendError
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.{Schedulers, SystemTime, Time}
import com.wavesplatform.{EitherMatchers, NTPTime}
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration.DurationInt
import scala.util.Random

class BlockchainUpdaterImplSpec extends FreeSpec with EitherMatchers with WithDomain with NTPTime with DBCacheSettings {
  import DomainPresets.*

  private val FEE_AMT = 1000000L

  def baseTest(setup: Time => (KeyPair, Seq[Block]), enableNg: Boolean = false, triggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop)(
      f: (BlockchainUpdaterImpl, KeyPair) => Unit
  ): Unit = withDomain(if (enableNg) NG else SettingsFromDefaultConfig) { d =>
    d.triggers = d.triggers :+ triggers

    val (account, blocks) = setup(ntpTime)

    blocks.foreach { block =>
      d.appendBlock(block)
    }

    f(d.blockchainUpdater, account)
  }

  def createTransfer(master: KeyPair, recipient: Address, ts: Long): TransferTransaction =
    TxHelpers.transfer(master, recipient, ENOUGH_AMT / 5, fee = 1000000, timestamp = ts, version = TxVersion.V1)

  def commonPreconditions(ts: Long): (KeyPair, List[Block]) = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis      = TxHelpers.genesis(master.toAddress, timestamp = ts)
    val genesisBlock = TestBlock.create(ts, Seq(genesis)).block
    val b1 = TestBlock
      .create(
        ts + 10,
        genesisBlock.id(),
        Seq(
          createTransfer(master, recipient.toAddress, ts + 1),
          createTransfer(master, recipient.toAddress, ts + 2),
          createTransfer(recipient, master.toAddress, ts + 3),
          createTransfer(master, recipient.toAddress, ts + 4),
          createTransfer(master, recipient.toAddress, ts + 5)
        )
      )
      .block
    val b2 = TestBlock
      .create(
        ts + 20,
        b1.id(),
        Seq(
          createTransfer(master, recipient.toAddress, ts + 11),
          createTransfer(recipient, master.toAddress, ts + 12),
          createTransfer(recipient, master.toAddress, ts + 13),
          createTransfer(recipient, master.toAddress, ts + 14)
        )
      )
      .block

    (master, List(genesisBlock, b1, b2))
  }

  "blockchain update events sending" - {
    "without NG" - {
      "genesis block and two transfers blocks" in {
        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = false, BlockchainUpdateTriggers.noop)((_, _) => ())
      }
    }

    "with NG" - {
      "genesis block and two transfers blocks" in {

        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = true, BlockchainUpdateTriggers.noop)((_, _) => ())
      }

      "block, then 2 microblocks, then block referencing previous microblock" in withDomain(NG) { d =>
        def preconditions(ts: Long): (Transaction, Seq[Transaction]) = {
          val master    = TxHelpers.signer(1)
          val recipient = TxHelpers.signer(2)

          val genesis = TxHelpers.genesis(master.toAddress, timestamp = ts)
          val transfers = Seq(
            createTransfer(master, recipient.toAddress, ts + 1),
            createTransfer(master, recipient.toAddress, ts + 2),
            createTransfer(master, recipient.toAddress, ts + 3),
            createTransfer(recipient, master.toAddress, ts + 4),
            createTransfer(master, recipient.toAddress, ts + 5)
          )

          (genesis, transfers)
        }

        d.triggers = d.triggers :+ BlockchainUpdateTriggers.noop

        val (genesis, transfers)       = preconditions(0)
        val (block1, microBlocks1And2) = chainBaseAndMicro(randomSig, genesis, Seq(transfers.take(2), Seq(transfers(2))))
        val (block2, microBlock3)      = chainBaseAndMicro(microBlocks1And2.head.totalResBlockSig, transfers(3), Seq(Seq(transfers(4))))

        d.blockchainUpdater.processBlock(block1) should beRight
        d.blockchainUpdater.processMicroBlock(microBlocks1And2.head, None) should beRight
        d.blockchainUpdater.processMicroBlock(microBlocks1And2.last, None) should beRight
        d.blockchainUpdater.processBlock(block2) should beRight // this should remove previous microblock
        d.blockchainUpdater.processMicroBlock(microBlock3.head, None) should beRight
        d.blockchainUpdater.shutdown()
      }
    }

    "VRF" in {
      val dapp   = KeyPair(Longs.toByteArray(Random.nextLong()))
      val sender = KeyPair(Longs.toByteArray(Random.nextLong()))

      withDomain(
        RideV4,
        balances = Seq(AddrWithBalance(dapp.toAddress, 10_00000000), AddrWithBalance(sender.toAddress, 10_00000000))
      ) { d =>
        val script = ScriptCompiler
          .compile(
            """
              |
              |{-# STDLIB_VERSION 4 #-}
              |{-# SCRIPT_TYPE ACCOUNT #-}
              |{-# CONTENT_TYPE DAPP #-}
              |
              |@Callable(i)
              |func default() = {
              |  [
              |    BinaryEntry("vrf", value(value(blockInfoByHeight(height)).vrf))
              |  ]
              |}
              |""".stripMargin,
            ScriptEstimatorV2
          )
          .explicitGet()
          ._1

        d.appendBlock(
          SetScriptTransaction.selfSigned(2.toByte, dapp, Some(script), 500_0000L, ntpTime.getTimestamp()).explicitGet()
        )

        val invoke =
          Signed.invokeScript(3.toByte, sender, dapp.toAddress, None, Seq.empty, 50_0000L, Waves, ntpTime.getTimestamp())

        d.appendBlock(d.createBlock(5.toByte, Seq(invoke)))
      }
    }
  }

  "BlockchainUpdater should replace current liquid block with better one" in {
    val currentBlockSender = TxHelpers.signer(1)
    val anotherBlockSender = TxHelpers.signer(2)

    withDomain(ConsensusImprovements, AddrWithBalance.enoughBalances(currentBlockSender, anotherBlockSender)) { d =>
      val parent = d.appendBlock()

      val betterBlock =
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = anotherBlockSender, ref = Some(parent.id()))
      val currentBlock =
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = currentBlockSender, ref = Some(parent.id()))
      val worseBlock =
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = anotherBlockSender, ref = Some(parent.id()))

      betterBlock.header.timestamp < currentBlock.header.timestamp shouldBe true
      currentBlock.header.timestamp < worseBlock.header.timestamp shouldBe true

      d.appendBlockE(currentBlock) should beRight

      val scheduler = Schedulers.singleThread("appender")
      val appender =
        BlockAppender(d.blockchainUpdater, SystemTime, d.utxPool, d.posSelector, scheduler, verify = false)(_, None)

      appender(worseBlock).runSyncUnsafe(1.minute) shouldBe Left(
        BlockAppendError(
          s"Competitors liquid block $worseBlock(timestamp=${worseBlock.header.timestamp}) is not better than existing (ng.base $currentBlock(timestamp=${currentBlock.header.timestamp}))",
          worseBlock
        )
      )

      appender(betterBlock).runSyncUnsafe(1.minute) should beRight
      d.lastBlock shouldBe betterBlock
      scheduler.shutdown()
    }
  }
}
