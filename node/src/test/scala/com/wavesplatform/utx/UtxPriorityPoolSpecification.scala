package com.wavesplatform.utx

import cats.data.NonEmptyList
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.{MultiDimensionalMiningConstraint, OneDimensionalMiningConstraint, TxEstimators}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool.PackStrategy
import com.wavesplatform.{BlocksTransactionsHelpers, EitherMatchers, NoShrink, TestValues, TransactionGen}
import org.scalacheck.Gen
import org.scalacheck.Gen.chooseNum
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.PatienceConfiguration.{Interval, Timeout}
import org.scalatest.{EitherValues, FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.duration._

class UtxPriorityPoolSpecification
    extends FreeSpec
    with Matchers
    with EitherMatchers
    with MockFactory
    with ScalaCheckDrivenPropertyChecks
    with TransactionGen
    with NoShrink
    with BlocksTransactionsHelpers
    with WithDomain
    with EitherValues
    with Eventually {
  "priority pool" - {
    import TestValues.{script => testScript, scriptComplexity => testScriptComplexity}
    implicit class UtxPoolImplExt(utx: UtxPoolImpl) {
      def setPriorityTxs(txs: Seq[Transaction]): Unit = {
        val asDiffs = txs.map {
          case tt: TransferTransaction =>
            val pfs = Map(
              tt.sender.toAddress                -> -(tt.fee + tt.amount),
              tt.recipient.asInstanceOf[Address] -> tt.amount
            ).view.mapValues(Portfolio.waves).toMap
            Diff(portfolios = pfs).bindTransaction(tt)

          case _ => Diff.empty
        }
        utx.setPriorityDiffs(asDiffs)
      }
    }

    def assertPortfolios(utx: UtxPool, transactions: Seq[TransferTransaction]): Unit = {
      val portfolios = transactions.groupBy(_.sender.toAddress).map {
        case (addr, transactions) =>
          val amt = transactions.map(tx => -(tx.amount + tx.fee)).sum
          (addr, amt)
      }
      portfolios.foreach {
        case (addr, balance) =>
          val pf = utx.pessimisticPortfolio(addr)
          pf.balance shouldBe balance
      }
    }

    val gen = for {
      acc         <- accountGen
      acc1        <- accountGen
      acc2        <- accountGen
      tx1         <- transferV2(acc, ENOUGH_AMT / 3, ntpTime)
      nonScripted <- Gen.nonEmptyListOf(transferV2(acc1, 10000000L, ntpTime).suchThat(_.fee < tx1.fee))
      scripted    <- Gen.nonEmptyListOf(transferV2(acc2, 10000000L, ntpTime).suchThat(_.fee < tx1.fee))
    } yield (tx1, nonScripted, scripted)

    def createState(scripted: Address, settings: WavesSettings = WavesSettings.default(), setBalance: Boolean = true): Blockchain = {
      val blockchain = stub[Blockchain]
      (() => blockchain.settings).when().returning(settings.blockchainSettings)
      (() => blockchain.height).when().returning(1)
      (() => blockchain.activatedFeatures).when().returning(Map(BlockchainFeatures.SmartAccounts.id -> 0))

      if (setBalance) (blockchain.balance _).when(*, *).returning(ENOUGH_AMT)
      (blockchain.leaseBalance _).when(*).returning(LeaseBalance(0, 0))

      (blockchain.accountScript _)
        .when(scripted)
        .returns(Some(AccountScriptInfo(PublicKey(new Array[Byte](32)), testScript, testScriptComplexity, Map.empty)))
      (blockchain.accountScript _).when(*).returns(None)
      val tb = TestBlock.create(Nil)
      (blockchain.blockHeader _).when(*).returning(Some(SignedBlockHeader(tb.header, tb.signature)))
      (blockchain.transactionMeta _).when(*).returning(None)
      blockchain
    }

    "preserves correct order of transactions" in forAll(gen) {
      case (tx1, nonScripted, scripted) =>
        val blockchain = createState(scripted.head.sender.toAddress)
        val utx =
          new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
        utx.putIfNew(tx1).resultE should beRight
        val minedTxs = scripted ++ nonScripted
        utx.setPriorityTxs(minedTxs)

        utx
          .packUnconfirmed(
            MultiDimensionalMiningConstraint(NonEmptyList.of(OneDimensionalMiningConstraint(1, TxEstimators.one, ""))),
            PackStrategy.Unlimited
          )
          ._1 shouldBe Some(minedTxs.head +: Nil)
        val expectedTxs = minedTxs :+ tx1
        utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(expectedTxs)
        utx.all shouldBe expectedTxs
        assertPortfolios(utx, expectedTxs)

        val (left, right) = minedTxs.splitAt(minedTxs.length / 2)

        utx.removeAll(left)
        utx.priorityPool.priorityTransactions should not be empty

        val expectedTxs1 = right :+ tx1
        assertPortfolios(utx, expectedTxs1)
        all(right.map(utx.putIfNew(_).resultE)) shouldBe Right(false)
        val test = utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1
        test shouldBe Some(expectedTxs1)
        utx.all shouldBe expectedTxs1
        assertPortfolios(utx, expectedTxs1)

        val expectedTxs2 = expectedTxs1 ++ left.sorted(TransactionsOrdering.InUTXPool(Set()))
        utx.removeAll(expectedTxs2)
        left.foreach(utx.putIfNew(_).resultE should beRight)
        utx.setPriorityTxs(expectedTxs1)
        utx.all shouldBe expectedTxs2
        assertPortfolios(utx, expectedTxs2)

        utx.removeAll(expectedTxs2)
        utx.all shouldBe empty
        utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe empty
        all(expectedTxs2.map(tx => utx.pessimisticPortfolio(tx.sender.toAddress))) shouldBe empty
    }

    "removes priority transactions from ordinary pool on pack" in forAll(gen) {
      case (_, nonScripted, scripted) =>
        val blockchain = createState(scripted.head.sender.toAddress)
        val utx =
          new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)

        utx.setPriorityTxs(nonScripted)
        nonScripted.foreach(utx.putIfNew(_).resultE should beRight)
        utx.nonPriorityTransactions shouldBe empty
        utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(nonScripted)
        utx.nonPriorityTransactions shouldBe empty
    }

    val genDependent = for {
      acc  <- accountGen
      acc1 <- accountGen
      tx1  <- transferV2WithRecipient(acc, acc1.publicKey, ENOUGH_AMT / 3, ntpTime).suchThat(_.amount > 20000000L)
      tx2  <- transferV2(acc1, tx1.amount / 2, ntpTime)
    } yield (tx1, tx2)

    "takes into account priority txs when pack" in forAll(genDependent) {
      case (tx1, tx2) =>
        val blockchain = createState(tx1.sender.toAddress, setBalance = false)
        (blockchain.balance _).when(tx1.sender.toAddress, *).returning(ENOUGH_AMT)
        (blockchain.balance _).when(*, *).returning(0) // Should be overriden in composite blockchain

        val utx =
          new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
        utx.setPriorityTxs(Seq(tx1))
        utx.putNewTx(tx2, verify = false, forceValidate = false).resultE should beRight
        utx.nonPriorityTransactions shouldBe Seq(tx2)
        utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(tx1 :: tx2 :: Nil)
    }

    "counts microblock size from priority diffs" in {
      val blockchain = createState(TxHelpers.defaultSigner.toAddress)
      val utx =
        new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)

      def createDiff(): Diff =
        Monoid.combineAll((1 to 5).map(_ => Diff.empty.bindTransaction(TxHelpers.issue())))

      utx.setPriorityDiffs(Seq(createDiff(), createDiff())) // 10 total
      utx.priorityPool.nextMicroBlockSize(3) shouldBe 5
      utx.priorityPool.nextMicroBlockSize(5) shouldBe 5
      utx.priorityPool.nextMicroBlockSize(8) shouldBe 5
      utx.priorityPool.nextMicroBlockSize(10) shouldBe 10
      utx.priorityPool.nextMicroBlockSize(12) shouldBe 12
    }

    "runs cleanup on priority pool" in forAll(genDependent) {
      case (tx1, tx2) =>
        val blockchain = createState(tx1.sender.toAddress, setBalance = false)
        (blockchain.balance _).when(*, *).returning(0) // All invalid

        val utx =
          new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
        utx.setPriorityTxs(Seq(tx1, tx2))
        utx.runCleanup()

        eventually(Timeout(5 seconds), Interval(50 millis))(utx.all shouldBe empty)
    }

    "invalidates priority pool on different microblock" in forAll(genDependent) {
      case (tx1, tx2) =>
        val blockchain = createState(tx1.sender.toAddress, setBalance = false)
        (blockchain.balance _).when(TxHelpers.defaultSigner.toAddress, *).returning(ENOUGH_AMT)
        (blockchain.balance _).when(*, *).returning(0L)

        val utx =
          new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)

        utx.setPriorityTxs(Seq(tx1, tx2))
        utx.removeAll(Seq(TxHelpers.issue()))

        utx.priorityPool.validPriorityDiffs shouldBe empty
        utx.priorityPool.priorityTransactions shouldBe Seq(tx1, tx2)

        val profitableTx = TxHelpers.issue()
        utx.putIfNew(profitableTx).resultE should beRight

        utx.all shouldBe Seq(tx1, tx2, profitableTx)
        utx.putIfNew(tx2).resultE should beLeft // Diff not counted
    }

    "takes into account priority diff on putIfNew" in forAll(genDependent) {
      case (tx1, tx2) =>
        val blockchain = createState(tx1.sender.toAddress, setBalance = false)
        (blockchain.balance _).when(tx1.sender.toAddress, *).returning(ENOUGH_AMT)
        (blockchain.balance _).when(tx2.sender.toAddress, *).returning(ENOUGH_AMT).noMoreThanOnce()
        (blockchain.balance _).when(tx2.sender.toAddress, *).returning(0)

        val utx =
          new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
        utx.setPriorityTxs(Seq(tx1))
        utx.putNewTx(tx2, true, false).resultE.explicitGet()
        utx.nonPriorityTransactions shouldBe Seq(tx2)
        utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(tx1 :: tx2 :: Nil)
    }
  }

  private def transferV2(sender: KeyPair, maxAmount: Long, time: Time) =
    (for {
      amount    <- chooseNum(1, (maxAmount * 0.9).toLong)
      recipient <- accountGen
      fee       <- chooseNum(ScriptExtraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransaction
      .selfSigned(TxVersion.V2, sender, recipient.toAddress, Waves, amount, Waves, fee, ByteStr.empty, time.getTimestamp())
      .explicitGet())
      .label("transferTransactionV2")

  private def transferV2WithRecipient(sender: KeyPair, recipient: PublicKey, maxAmount: Long, time: Time) =
    (for {
      amount <- chooseNum(1, (maxAmount * 0.9).toLong)
      fee    <- chooseNum(ScriptExtraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransaction
      .selfSigned(TxVersion.V2, sender, recipient.toAddress, Waves, amount, Waves, fee, ByteStr.empty, time.getTimestamp())
      .explicitGet())
      .label("transferWithRecipient")
}
