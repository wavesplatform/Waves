package com.wavesplatform

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.typesafe.config.ConfigFactory
import com.wavesplatform.history.{HistoryWriterImpl, StorageFactory}
import com.wavesplatform.settings.{BlockchainSettings, FeeSettings, FeesSettings, FunctionalitySettings, UtxSettings, WavesSettings}
import com.wavesplatform.state2.diffs._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{FeeCalculator, PaymentTransaction, Transaction}
import scorex.utils.Time

import scala.concurrent.duration._

class UtxPoolSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with TransactionGen
  with NoShrink {

  private val calculator = new FeeCalculator(FeesSettings(Map(
    1 -> List(FeeSettings("", 0)),
    2 -> List(FeeSettings("", 0)),
    4 -> List(FeeSettings("", 0))
  )))

  private def mkState(senderAccount: Address, senderBalance: Long) = {

    val config = ConfigFactory.load()
    val genesisSettings = TestHelpers.genesisSettings(Map(senderAccount -> senderBalance))
    val settings = WavesSettings.fromConfig(config).copy(blockchainSettings = BlockchainSettings(None, None, false, None, 'T', 5, 5, FunctionalitySettings.TESTNET, genesisSettings))

    val (history, _, _, state, bcu, _) = StorageFactory(settings).get

    bcu.processBlock(Block.genesis(genesisSettings).right.get)

    (state, history)
  }

  private def transfer(sender: PrivateKeyAccount, maxAmount: Long, time: Time) = (for {
    amount <- chooseNum(1, (maxAmount * 0.9).toLong)
    recipient <- accountGen
    fee <- chooseNum(1, (maxAmount * 0.1).toLong)
  } yield TransferTransaction.create(None, sender, recipient, amount, time.getTimestamp(), None, fee, Array.empty[Byte]).right.get)
    .label("transferTransaction")

  private def transferWithRecipient(sender: PrivateKeyAccount, recipient: PublicKeyAccount, maxAmount: Long, time: Time) = (for {
    amount <- chooseNum(1, (maxAmount * 0.9).toLong)
    fee <- chooseNum(1, (maxAmount * 0.1).toLong)
  } yield TransferTransaction.create(None, sender, recipient, amount, time.getTimestamp(), None, fee, Array.empty[Byte]).right.get)
    .label("transferWithRecipient")

  private val stateGen = for {
    sender <- accountGen.label("sender")
    senderBalance <- positiveLongGen.label("senderBalance")
  } yield {
    val (state, history) = mkState(sender, senderBalance)
    (sender, senderBalance, state, history)
  }
  private val twoOutOfManyValidPayments = (for {
    (sender, senderBalance, state, history) <- stateGen
    recipient <- accountGen
    n <- chooseNum(3, 10)
    fee <- chooseNum(1, (senderBalance * 0.01).toLong)
    offset <- chooseNum(1000L, 2000L)
  } yield {
    val time = new TestTime()
    val utx = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, UtxSettings(10, 10.minutes))
    val amountPart = (senderBalance - fee) / 2 - fee
    val txs = for (_ <- 1 to n) yield PaymentTransaction.create(sender, recipient, amountPart, fee, time.getTimestamp()).right.get
    (utx, time, txs, (offset + 1000).millis)
  }).label("twoOutOfManyValidPayments")

  private val emptyUtxPool = stateGen
    .map { case (sender, senderBalance, state, history) =>
      val time = new TestTime()
      val utxPool = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, UtxSettings(10, 1.minute))
      (sender, state, utxPool)
    }
    .label("emptyUtxPool")

  private val withValidPayments = (for {
    (sender, senderBalance, state, history) <- stateGen
    recipient <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time))
  } yield {
    val settings = UtxSettings(10, 1.minute)
    val utxPool = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, settings)
    txs.foreach(utxPool.putIfNew)
    (sender, state, utxPool, time, settings)
  }).label("withValidPayments")

  private def utxTest(utxSettings: UtxSettings = UtxSettings(20, 5.seconds), txCount: Int = 10)
                     (f: (Seq[TransferTransaction], UtxPool, TestTime) => Unit): Unit = forAll(
    stateGen,
    chooseNum(2, txCount).label("txCount")) { case ((sender, senderBalance, state, history), count) =>
    val time = new TestTime()

    forAll(listOfN(count, transfer(sender, senderBalance / 2, time))) { txs =>
      val utx = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, utxSettings)
      f(txs, utx, time)
    }
  }

  private val dualTxGen: Gen[(UtxPool, TestTime, Seq[Transaction], FiniteDuration, Seq[Transaction])] =
    for {
      (sender, senderBalance, state, history) <- stateGen
      ts = System.currentTimeMillis()
      count1 <- chooseNum(5, 10)
      tx1 <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts)))
      offset <- chooseNum(5000L, 10000L)
      tx2 <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts + offset + 1000)))
    } yield {
      val time = new TestTime()
      val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Stub,
        TestFunctionalitySettings.EmptyFeaturesSettings).get
      val utx = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, UtxSettings(10, offset.millis))
      (utx, time, tx1, (offset + 1000).millis, tx2)
    }

  "UTX Pool" - {
    "does not add new transactions when full" in utxTest(UtxSettings(1, 5.seconds)) { (txs, utx, _) =>
      utx.putIfNew(txs.head) shouldBe 'right
      all(txs.tail.map(t => utx.putIfNew(t))) should produce("pool size limit")
    }

    "does not broadcast the same transaction twice" in utxTest() { (txs, utx, _) =>
      utx.putIfNew(txs.head) shouldBe 'right
      utx.putIfNew(txs.head) shouldBe 'right
    }

    "evicts expired transactions when removeAll is called" in forAll(dualTxGen) { case (utx, time, txs1, offset, txs2) =>
      all(txs1.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all.size shouldEqual txs1.size

      time.advance(offset)
      utx.removeAll(Seq.empty)

      all(txs2.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all.size shouldEqual txs2.size
    }

    "evicts expired transactions when packUnconfirmed is called" in forAll(dualTxGen) { case (utx, time, txs, offset, _) =>
      all(txs.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all.size shouldEqual txs.size

      time.advance(offset)

      utx.packUnconfirmed(100, false) shouldBe 'empty
      utx.all shouldBe 'empty
    }

    "evicts one of mutually invalid transactions when packUnconfirmed is called" in forAll(twoOutOfManyValidPayments) { case (utx, time, txs, offset) =>
      all(txs.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all.size shouldEqual txs.size

      time.advance(offset)

      utx.packUnconfirmed(100, false).size shouldBe 2
      utx.all.size shouldBe 2
    }

    "portfolio" - {
      "returns a count of assets from the state if there is no transaction" in forAll(emptyUtxPool) { case (sender, state, utxPool) =>
        val basePortfolio = state().accountPortfolio(sender)

        utxPool.size shouldBe 0
        val utxPortfolio = utxPool.portfolio(sender)

        basePortfolio shouldBe utxPortfolio
      }

      "taking into account unconfirmed transactions" in forAll(withValidPayments) { case (sender, state, utxPool, _, _) =>
        val basePortfolio = state().accountPortfolio(sender)

        utxPool.size should be > 0
        val utxPortfolio = utxPool.portfolio(sender)

        utxPortfolio.balance should be <= basePortfolio.balance
        utxPortfolio.leaseInfo.leaseOut should be <= basePortfolio.leaseInfo.leaseOut
        // should not be changed
        utxPortfolio.leaseInfo.leaseIn shouldBe basePortfolio.leaseInfo.leaseIn
        utxPortfolio.assets.foreach { case (assetId, count) =>
          count should be <= basePortfolio.assets.getOrElse(assetId, count)
        }
      }

      "is changed after transactions with these assets are removed" in forAll(withValidPayments) { case (sender, _, utxPool, time, settings) =>
        val utxPortfolioBefore = utxPool.portfolio(sender)
        val poolSizeBefore = utxPool.size

        time.advance(settings.maxTransactionAge * 2)
        utxPool.packUnconfirmed(100, false)

        poolSizeBefore should be > utxPool.size
        val utxPortfolioAfter = utxPool.portfolio(sender)

        utxPortfolioAfter.balance should be >= utxPortfolioBefore.balance
        utxPortfolioAfter.leaseInfo.leaseOut should be >= utxPortfolioBefore.leaseInfo.leaseOut
        utxPortfolioAfter.assets.foreach { case (assetId, count) =>
          count should be >= utxPortfolioBefore.assets.getOrElse(assetId, count)
        }
      }
    }
  }
}
