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
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.Time
import com.wavesplatform.{EitherMatchers, NTPTime}
import org.scalamock.scalatest.MockFactory

import scala.util.Random

class BlockchainUpdaterImplSpec extends FreeSpec with EitherMatchers with WithDomain with NTPTime with DBCacheSettings with MockFactory {
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
    val genesisBlock = TestBlock.create(ts, Seq(genesis))
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
    val b2 = TestBlock.create(
      ts + 20,
      b1.id(),
      Seq(
        createTransfer(master, recipient.toAddress, ts + 11),
        createTransfer(recipient, master.toAddress, ts + 12),
        createTransfer(recipient, master.toAddress, ts + 13),
        createTransfer(recipient, master.toAddress, ts + 14)
      )
    )

    (master, List(genesisBlock, b1, b2))
  }

  "blockchain update events sending" - {
    "without NG" - {
      "genesis block and two transfers blocks" in {
        val triggersMock = mock[BlockchainUpdateTriggers]

        inSequence {
          (triggersMock.onProcessBlock _)
            .expects(where { (block, snapshot, _, _, bc) =>
              bc.height == 0 &&
              block.transactionData.length == 1 &&
              snapshot.balances.isEmpty &&
              snapshot.transactions.head._2.snapshot.balances.head._2 == ENOUGH_AMT
            })
            .once()

          (triggersMock.onProcessBlock _)
            .expects(where { (block, snapshot, _, _, bc) =>
              val txInfo = snapshot.transactions.head
              val tx     = txInfo._2.transaction.asInstanceOf[TransferTransaction]

              bc.height == 1 &&
              block.transactionData.length == 5 &&
              // miner reward, no NG — all txs fees
              snapshot.balances.size == 1 &&
              snapshot.balances.head._2 == FEE_AMT * 5 &&
              // first Tx updated balances
              snapshot.transactions.head._2.snapshot.balances((tx.recipient.asInstanceOf[Address], Waves)) == (ENOUGH_AMT / 5) &&
              snapshot.transactions.head._2.snapshot.balances((tx.sender.toAddress, Waves)) == ENOUGH_AMT - ENOUGH_AMT / 5 - FEE_AMT
            })
            .once()

          (triggersMock.onProcessBlock _).expects(*, *, *, *, *).once()
        }

        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = false, triggersMock)((_, _) => ())
      }
    }

    "with NG" - {
      "genesis block and two transfers blocks" in {
        val triggersMock = mock[BlockchainUpdateTriggers]

        inSequence {
          (triggersMock.onProcessBlock _)
            .expects(where { (block, snapshot, _, _, bc) =>
              bc.height == 0 &&
              block.transactionData.length == 1 &&
              snapshot.balances.isEmpty &&
              snapshot.transactions.head._2.snapshot.balances.head._2 == ENOUGH_AMT
            })
            .once()

          (triggersMock.onProcessBlock _)
            .expects(where { (block, snapshot, _, _, bc) =>
              bc.height == 1 &&
              block.transactionData.length == 5 &&
              snapshot.balances.isEmpty // no txs with fee in previous block
            })
            .once()

          (triggersMock.onProcessBlock _)
            .expects(where { (block, snapshot, _, _, bc) =>
              bc.height == 2 &&
              block.transactionData.length == 4 &&
              snapshot.balances.size == 1 &&
              snapshot.balances.head._2 == FEE_AMT * 5 // all fee from previous block
            })
            .once()
        }

        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = true, triggersMock)((_, _) => ())
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

        val triggersMock = mock[BlockchainUpdateTriggers]

        d.triggers = d.triggers :+ triggersMock

        val (genesis, transfers)       = preconditions(0)
        val (block1, microBlocks1And2) = chainBaseAndMicro(randomSig, genesis, Seq(transfers.take(2), Seq(transfers(2))))
        val (block2, microBlock3)      = chainBaseAndMicro(microBlocks1And2.head.totalResBlockSig, transfers(3), Seq(Seq(transfers(4))))

        inSequence {
          // genesis
          (triggersMock.onProcessBlock _)
            .expects(where { (block, snapshot, _, _, bc) =>
              bc.height == 0 &&
              block.transactionData.length == 1 &&
              snapshot.balances.isEmpty &&
              snapshot.transactions.head._2.snapshot.balances.head._2 == ENOUGH_AMT
            })
            .once()

          // microblock 1
          (triggersMock.onProcessMicroBlock _)
            .expects(where { (microBlock, snapshot, bc, _, _) =>
              bc.height == 1 &&
              microBlock.transactionData.length == 2 &&
              snapshot.balances.isEmpty // no txs with fee in previous block
            })
            .once()

          // microblock 2
          (triggersMock.onProcessMicroBlock _)
            .expects(where { (microBlock, snapshot, bc, _, _) =>
              bc.height == 1 &&
              microBlock.transactionData.length == 1 &&
              snapshot.balances.isEmpty // no txs with fee in previous block
            })
            .once()

          // rollback microblock
          (triggersMock.onMicroBlockRollback _)
            .expects(where { (_, toSig) =>
              toSig == microBlocks1And2.head.totalResBlockSig
            })
            .once()

          // next keyblock
          (triggersMock.onProcessBlock _)
            .expects(where { (block, _, _, _, bc) =>
              bc.height == 1 &&
              block.header.reference == microBlocks1And2.head.totalResBlockSig
            })
            .once()

          // microblock 3
          (triggersMock.onProcessMicroBlock _)
            .expects(where { (microBlock, _, bc, _, _) =>
              bc.height == 2 && microBlock.reference == block2.signature
            })
            .once()
        }

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
}
