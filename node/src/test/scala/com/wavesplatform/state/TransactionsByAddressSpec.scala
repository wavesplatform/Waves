package com.wavesplatform.state

import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{GenesisSettings, GenesisTransactionSettings, TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.{BlockGen, NoShrink}
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

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
      .buildAndSign(3.toByte, ntpNow, reference, 1000, ByteStr(new Array[Byte](32)), transactions, sender, Seq.empty, -1L)
      .explicitGet()

  val gen = for {
    sender <- accountGen
    genesisTimestamp = ntpNow
    genesisBlock = Block
      .genesis(
        GenesisSettings(
          genesisTimestamp,
          genesisTimestamp,
          diffs.ENOUGH_AMT,
          None,
          Seq(GenesisTransactionSettings(sender.toAddress.stringRepr, diffs.ENOUGH_AMT)),
          1000,
          1.minute
        )
      )
      .explicitGet()
    recipient1    <- accountGen
    recipient2    <- accountGen
    txCount1      <- Gen.choose(10, 50)
    transactions1 <- Gen.listOfN(txCount1, transferGen(sender, Gen.oneOf(recipient1, recipient2).map(_.toAddress)))
    block1 = mkBlock(sender, genesisBlock.id(), transactions1)
    txCount2      <- Gen.choose(10, 50)
    transactions2 <- Gen.listOfN(txCount2, transferGen(sender, Gen.oneOf(recipient1, recipient2).map(_.toAddress)))
    block2 = mkBlock(sender, block1.id(), transactions2)
  } yield {
    (sender, recipient1, recipient2, Seq(genesisBlock, block1, block2))
  }

  private def test(f: (Address, Seq[Block], Domain) => Unit)(implicit pos: Position): Unit = {
    forAll(gen) {
      case (sender, r1, r2, blocks) =>
        val settings = WavesSettings
          .default()
          .copy(blockchainSettings = WavesSettings.default().blockchainSettings.copy(functionalitySettings = TestFunctionalitySettings.Enabled))
        withDomain(settings) { d =>
          for (b <- blocks) {
            d.blockchainUpdater.processBlock(b, b.header.generationSignature, verify = false) shouldBe 'right
          }

          Seq[Address](sender, r1, r2).foreach(f(_, blocks, d))

          d.blockchainUpdater.processBlock(
            TestBlock.create(System.currentTimeMillis(), blocks.last.signature, Seq.empty),
            new Array[Byte](32),
            verify = false
          )

          Seq[Address](sender, r1, r2).foreach(f(_, blocks, d))
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
        forAll(Gen.oneOf(senderTransactions.map(_._2))) { id =>
          transactionsAfter(id) shouldEqual d.addressTransactions(sender, Some(id)).map { case (h, tx) => h -> tx.id() }
        }
      }
    }
    "all transactions" in test { (sender, blocks, d) =>
      collectTransactions(sender, blocks) shouldEqual d.addressTransactions(sender).map { case (h, tx) => h -> tx.id() }
    }
  }
}
