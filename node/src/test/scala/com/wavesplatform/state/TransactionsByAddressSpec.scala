package com.wavesplatform.state

import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{Constants, GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TransactionParser}
import com.wavesplatform.{BlockGen, NoShrink}
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration._

class TransactionsByAddressSpec extends FreeSpec with ScalaCheckDrivenPropertyChecks with BlockGen with WithDomain with Matchers with NoShrink {
  def transferGen(sender: KeyPair, rs: Gen[AddressOrAlias]): Gen[TransferTransaction] =
    for {
      recipient <- rs
      t <- Gen.oneOf(
        transferGeneratorPV2(ntpTime.getTimestamp(), sender, recipient, 100000000000L),
        transferGeneratorP(ntpTime.getTimestamp(), sender, recipient, 100000000000L)
      )
    } yield t

  def mkBlock(sender: KeyPair, reference: ByteStr, transactions: Seq[Transaction]): Block =
    Block
      .buildAndSign(3.toByte, ntpNow, reference, NxtLikeConsensusBlockData(1000, ByteStr(new Array[Byte](32))), transactions, sender, Set.empty)
      .explicitGet()

  val gen = for {
    sender <- accountGen
    genesisTimestamp = ntpNow
    genesisBlock = Block
      .genesis(
        GenesisSettings(
          genesisTimestamp,
          genesisTimestamp,
          Constants.TotalWaves,
          None,
          Seq(GenesisTransactionSettings(sender.toAddress.stringRepr, Constants.TotalWaves)),
          1000,
          1.minute
        )
      )
      .explicitGet()
    recipient1    <- accountGen
    recipient2    <- accountGen
    txCount1      <- Gen.choose(10, 50)
    transactions1 <- Gen.listOfN(txCount1, transferGen(sender, Gen.oneOf(recipient1, recipient2).map(_.toAddress)))
    block1 = mkBlock(sender, genesisBlock.uniqueId, transactions1)
    txCount2      <- Gen.choose(10, 50)
    transactions2 <- Gen.listOfN(txCount2, transferGen(sender, Gen.oneOf(recipient1, recipient2).map(_.toAddress)))
    block2 = mkBlock(sender, block1.uniqueId, transactions2)
  } yield {
    (sender, recipient1, recipient2, Seq(genesisBlock, block1, block2))
  }

  private def test(f: (Address, Seq[Block], Domain) => Unit)(implicit pos: Position): Unit = {
    forAll(gen) {
      case (sender, r1, r2, blocks) =>
        withDomain() { d =>
          for (b <- blocks) {
            d.blockchainUpdater.processBlock(b, verify = false)
          }

          Seq[Address](sender, r1, r2).foreach(f(_, blocks, d))
        }
    }
  }

  private def collectTransactions(forAddress: Address, fromBlocks: Seq[Block]): Seq[(Int, ByteStr)] =
    fromBlocks.zipWithIndex.flatMap { case (b, h) => b.transactionData.map(t => (h + 1, t)) }.collect {
      case (h, t: TransferTransaction) if t.sender.toAddress == forAddress || t.recipient == forAddress => (h, t.id())
      case (h, g: GenesisTransaction) if g.recipient == forAddress                                      => (h, g.id())
    }

  private def transactionsFromBlockchain(
      blockchain: Blockchain,
      sender: Address,
      types: Set[TransactionParser] = Set.empty,
      fromId: Option[ByteStr] = None
  ): Seq[(Int, ByteStr)] =
    Await
      .result(blockchain.addressTransactionsObservable(sender, types, fromId).toListL.runToFuture, Duration.Inf)
      .map { case (h, tx) => (h, tx.id()) }
      .reverse

  "Transactions by address returns" - {
    "correct N txs on request" - {
      "with `after`" in pendingUntilFixed(test { (sender, blocks, d) =>
        val senderTransactions                                  = collectTransactions(sender, blocks)
        def transactionsAfter(id: ByteStr): Seq[(Int, ByteStr)] = senderTransactions.dropWhile { case (_, txId) => txId != id }.tail
        forAll(Gen.oneOf(senderTransactions.map(_._2))) { id =>
          transactionsAfter(id) shouldEqual transactionsFromBlockchain(d.blockchainUpdater, sender, fromId = Some(id))
        }
      })
    }
    "all transactions" in pendingUntilFixed(test { (sender, blocks, d) =>
      collectTransactions(sender, blocks) shouldEqual transactionsFromBlockchain(d.blockchainUpdater, sender)
    })
  }
}
