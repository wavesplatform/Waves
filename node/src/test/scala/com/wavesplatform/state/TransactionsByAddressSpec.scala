package com.wavesplatform.state

import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{InterferableDB, WithDomain}
import com.wavesplatform.history.Domain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{Constants, GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TransactionType, TxHelpers, TxVersion}
import com.wavesplatform.BlockGen
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.test.DomainPresets.RideV5
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers.{defaultAddress, issue, secondSigner}
import org.scalactic.source.Position

import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import scala.concurrent.duration.Duration.Inf

class TransactionsByAddressSpec extends FreeSpec with BlockGen with WithDomain {
  def transfers(sender: KeyPair, rs: AddressOrAlias, amount: Long): Seq[TransferTransaction] =
    Seq(
      TxHelpers.transfer(sender, rs, amount),
      TxHelpers.transfer(sender, rs, amount, version = TxVersion.V1)
    )

  def mkBlock(sender: KeyPair, reference: ByteStr, transactions: Seq[Transaction]): Block =
    Block
      .buildAndSign(3.toByte, ntpNow, reference, 1000, ByteStr(new Array[Byte](32)), transactions, sender, Seq.empty, -1L, None, None)
      .explicitGet()

  val setup: Seq[(KeyPair, KeyPair, KeyPair, Seq[Block])] = {
    val sender     = TxHelpers.signer(1)
    val recipient1 = TxHelpers.signer(2)
    val recipient2 = TxHelpers.signer(3)

    val genesisTimestamp = ntpNow
    val genesisBlock = Block
      .genesis(
        GenesisSettings(
          genesisTimestamp,
          genesisTimestamp,
          Constants.TotalWaves,
          None,
          Seq(GenesisTransactionSettings(sender.toAddress.toString, Constants.TotalWaves)),
          1000,
          1.minute
        ),
        rideV6Activated = false,
        txStateSnapshotActivated = false
      )
      .explicitGet()

    val txCount1 = 20
    val txCount2 = 30

    Seq(recipient1, recipient2).map { recipient =>
      val transactions1 = (1 to txCount1 / 2).flatMap(_ => transfers(sender, recipient.toAddress, Constants.TotalWaves / 2 / txCount1))
      val block1        = mkBlock(sender, genesisBlock.id(), transactions1)
      val transactions2 = (1 to txCount2 / 2).flatMap(_ => transfers(sender, recipient.toAddress, Constants.TotalWaves / 2 / txCount2))
      val block2        = mkBlock(sender, block1.id(), transactions2)

      (sender, recipient1, recipient2, Seq(genesisBlock, block1, block2))
    }
  }

  private def test(f: (Address, Seq[Block], Domain) => Unit)(implicit pos: Position): Unit = {
    setup.foreach { case (sender, r1, r2, blocks) =>
      withDomain() { d =>
        for (b <- blocks) {
          d.blockchainUpdater.processBlock(b, b.header.generationSignature, verify = false)
        }

        Seq[Address](sender.toAddress, r1.toAddress, r2.toAddress).foreach(f(_, blocks, d))

        d.blockchainUpdater.processBlock(
          TestBlock.create(System.currentTimeMillis(), blocks.last.signature, Seq.empty),
          ByteStr(new Array[Byte](32)),
          verify = false
        )

        Seq[Address](sender.toAddress, r1.toAddress, r2.toAddress).foreach(f(_, blocks, d))
      }
    }
  }

  private def collectTransactions(forAddress: Address, fromBlocks: Seq[Block]): Seq[(Int, ByteStr)] =
    fromBlocks.zipWithIndex
      .flatMap { case (b, h) => b.transactionData.map(t => (h + 1, t)) }
      .collect {
        case (h, t: TransferTransaction) if t.sender.toAddress == forAddress || t.recipient == forAddress => (h, t.id())
        case (h, g: GenesisTransaction) if g.recipient == forAddress                                      => (h, g.id())
      }
      .reverse

  "Transactions by address returns" - {
    "correct N txs on request" - {
      "with `after`" in test { (sender, blocks, d) =>
        val senderTransactions                                  = collectTransactions(sender, blocks)
        def transactionsAfter(id: ByteStr): Seq[(Int, ByteStr)] = senderTransactions.dropWhile { case (_, txId) => txId != id }.tail
        senderTransactions.map(_._2).foreach { id =>
          transactionsAfter(id) shouldEqual d.addressTransactions(sender, Some(id)).map { case (h, tx) => h -> tx.id() }
        }
      }
    }
    "all transactions" in test { (sender, blocks, d) =>
      collectTransactions(sender, blocks) shouldEqual d.addressTransactions(sender).map { case (h, tx) => h -> tx.id() }
    }
    "distinct result avoiding inconsistent state" in {
      val startRead = new ReentrantLock()
      withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner), InterferableDB(_, startRead)) { d =>
        d.appendBlock()
        d.appendMicroBlock(issue())
        startRead.lock()
        val txs = Future { d.addressTransactions(defaultAddress).map(_._2.tpe) }
        d.blockchain.bestLiquidDiff.synchronized(d.appendKeyBlock())
        startRead.unlock()
        Await.result(txs, Inf).map(_.tpe) shouldBe List(TransactionType.Issue)
      }
    }
  }
}
