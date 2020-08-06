package com.wavesplatform.utx

import java.nio.file.{Files, Path}

import cats.data.NonEmptyList
import com.wavesplatform
import com.wavesplatform._
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.{PoSSelector, TransactionsOrdering}
import com.wavesplatform.database.{LevelDBWriter, TestStorageFactory, openDB}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.events.UtxEvent
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.history.randomSig
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.wavesplatform.mining._
import com.wavesplatform.network.{InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.settings._
import com.wavesplatform.state._
import com.wavesplatform.state.appender.{ExtensionAppender, MicroblockAppender}
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.utils.TestLevelDB
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.{GenericError, SenderIsBlacklisted}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, Transaction, _}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool.PackStrategy
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import monix.reactive.subjects.PublishSubject
import org.iq80.leveldb.DB
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.PatienceConfiguration.{Interval, Timeout}
import org.scalatest.{EitherValues, FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

private object UtxPoolSpecification {
  private val ignoreSpendableBalanceChanged = PublishSubject[(Address, Asset)]()

  final case class TempDB(fs: FunctionalitySettings, dbSettings: DBSettings) {
    val path: Path            = Files.createTempDirectory("leveldb-test")
    val db: DB                = openDB(path.toAbsolutePath.toString)
    val writer: LevelDBWriter = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, fs)

    sys.addShutdownHook {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }
}

class UtxPoolSpecification
    extends FreeSpec
    with Matchers
    with EitherMatchers
    with MockFactory
    with PropertyChecks
    with TransactionGen
    with NoShrink
    with BlocksTransactionsHelpers
    with WithDomain
    with EitherValues
    with Eventually {
  private val PoolDefaultMaxBytes = 50 * 1024 * 1024 // 50 MB

  import FeeValidation.{ScriptExtraFee => extraFee}
  import FunctionalitySettings.TESTNET.{maxTransactionTimeBackOffset => maxAge}
  import UtxPoolSpecification._

  private def mkBlockchain(senderAccount: Address, senderBalance: Long) = {
    val genesisSettings = TestHelpers.genesisSettings(Map(senderAccount -> senderBalance))
    val origSettings    = WavesSettings.default()
    val settings = origSettings.copy(
      blockchainSettings = BlockchainSettings(
        'T',
        FunctionalitySettings.TESTNET.copy(
          preActivatedFeatures = Map(
            BlockchainFeatures.MassTransfer.id  -> 0,
            BlockchainFeatures.SmartAccounts.id -> 0,
            BlockchainFeatures.Ride4DApps.id    -> 0
          )
        ),
        genesisSettings,
        RewardsSettings.TESTNET
      ),
      featuresSettings = origSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false)
    )

    val dbContext            = TempDB(settings.blockchainSettings.functionalitySettings, settings.dbSettings)
    val (bcu, levelDBWriter) = TestStorageFactory(settings, dbContext.db, new TestTime, ignoreSpendableBalanceChanged, ignoreBlockchainUpdateTriggers)
    bcu.processBlock(Block.genesis(genesisSettings).explicitGet()) should beRight
    bcu
  }

  private def transfer(sender: KeyPair, maxAmount: Long, time: Time) =
    (for {
      amount    <- chooseNum(1, (maxAmount * 0.9).toLong)
      recipient <- accountGen
      fee       <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransaction
      .selfSigned(1.toByte, sender, recipient.toAddress, Waves, amount, Waves, fee, ByteStr.empty, time.getTimestamp())
      .explicitGet())
      .label("transferTransaction")

  private def transferV2(sender: KeyPair, maxAmount: Long, time: Time) =
    (for {
      amount    <- chooseNum(1, (maxAmount * 0.9).toLong)
      recipient <- accountGen
      fee       <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransaction
      .selfSigned(TxVersion.V2, sender, recipient.toAddress, Waves, amount, Waves, fee, ByteStr.empty, time.getTimestamp())
      .explicitGet())
      .label("transferTransactionV2")

  private def transferWithRecipient(sender: KeyPair, recipient: PublicKey, maxAmount: Long, time: Time) =
    (for {
      amount <- chooseNum(1, (maxAmount * 0.9).toLong)
      fee    <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransaction
      .selfSigned(1.toByte, sender, recipient.toAddress, Waves, amount, Waves, fee, ByteStr.empty, time.getTimestamp())
      .explicitGet())
      .label("transferWithRecipient")

  private def transferV2WithRecipient(sender: KeyPair, recipient: PublicKey, maxAmount: Long, time: Time) =
    (for {
      amount <- chooseNum(1, (maxAmount * 0.9).toLong)
      fee    <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransaction
      .selfSigned(TxVersion.V2, sender, recipient.toAddress, Waves, amount, Waves, fee, ByteStr.empty, time.getTimestamp())
      .explicitGet())
      .label("transferWithRecipient")

  private def massTransferWithRecipients(sender: KeyPair, recipients: List[PublicKey], maxAmount: Long, time: Time) = {
    val amount    = maxAmount / (recipients.size + 1)
    val transfers = recipients.map(r => ParsedTransfer(r.toAddress, amount))
    val minFee    = FeeValidation.FeeConstants(TransferTransaction.typeId) + FeeValidation.FeeConstants(MassTransferTransaction.typeId) * transfers.size
    val txs = for { fee <- chooseNum(minFee, amount) } yield MassTransferTransaction
      .selfSigned(1.toByte, sender, Waves, transfers, fee, time.getTimestamp(), ByteStr.empty)
      .explicitGet()
    txs.label("transferWithRecipient")
  }

  private val stateGen = for {
    sender        <- accountGen.label("sender")
    senderBalance <- positiveLongGen.label("senderBalance")
    if senderBalance > 100000L
  } yield {
    val bcu = mkBlockchain(sender.toAddress, senderBalance)
    (sender, senderBalance, bcu)
  }

  private val stateWithTransfer = for {
    (sender, balance, bcu) <- stateGen
    time = new TestTime
    transfer <- transfer(sender, balance, time)
  } yield (time, bcu, transfer)

  private val twoOutOfManyValidPayments = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    n                            <- chooseNum(3, 10)
    fee                          <- chooseNum(extraFee, (senderBalance * 0.01).toLong)
    offset                       <- chooseNum(1000L, 2000L)
  } yield {
    val time = new TestTime()
    val utx =
      new UtxPoolImpl(
        time,
        bcu,
        ignoreSpendableBalanceChanged,
        UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
      )
    val amountPart = (senderBalance - fee) / 2 - fee
    val txs        = for (_ <- 1 to n) yield createWavesTransfer(sender, recipient.toAddress, amountPart, fee, time.getTimestamp()).explicitGet()
    (utx, time, txs, (offset + 1000).millis)
  }).label("twoOutOfManyValidPayments")

  private val emptyUtxPool = stateGen
    .map {
      case (sender, _, bcu) =>
        val time = new TestTime()
        val utxPool =
          new UtxPoolImpl(
            time,
            bcu,
            ignoreSpendableBalanceChanged,
            UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
          )
        (sender, bcu, utxPool)
    }
    .label("emptyUtxPool")

  private val withValidPayments = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient.publicKey, senderBalance / 10, time))
  } yield {
    val settings =
      UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    txs.foreach(utxPool.putIfNew(_))
    (sender, bcu, utxPool, time, settings)
  }).label("withValidPayments")

  private val withValidPaymentsNotAdded = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient.publicKey, senderBalance / 10, time))
  } yield {
    val settings =
      UtxSettings(txs.size, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    (sender, bcu, utxPool, txs, time, settings)
  }).label("withValidPayments")

  private val withBlacklisted = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient.publicKey, senderBalance / 10, time)) // @TODO: Random transactions
  } yield {
    val settings =
      UtxSettings(
        10,
        PoolDefaultMaxBytes,
        1000,
        Set(sender.toAddress.toString),
        Set.empty,
        allowTransactionsFromSmartAccounts = true,
        allowSkipChecks = false
      )
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    (sender, utxPool, txs)
  }).label("withBlacklisted")

  private val withBlacklistedAndAllowedByRule = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient.publicKey, senderBalance / 10, time)) // @TODO: Random transactions
  } yield {
    val settings =
      UtxSettings(
        txs.length,
        PoolDefaultMaxBytes,
        1000,
        Set(sender.toAddress.toString),
        Set(recipient.toAddress.toString),
        allowTransactionsFromSmartAccounts = true,
        allowSkipChecks = false
      )
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    (sender, utxPool, txs)
  }).label("withBlacklistedAndAllowedByRule")

  private def massTransferWithBlacklisted(allowRecipients: Boolean) =
    (for {
      (sender, senderBalance, bcu) <- stateGen
      addressesSize                <- Gen.choose(1, MassTransferTransaction.MaxTransferCount)
      addressGen = Gen.listOfN(addressesSize, accountGen).filter(list => if (allowRecipients) list.nonEmpty else true)
      recipients <- addressGen.map(_.map(_.publicKey))
      time = new TestTime()
      txs <- Gen.nonEmptyListOf(massTransferWithRecipients(sender, recipients, senderBalance / 10, time))
    } yield {
      val whitelist: Set[String] = if (allowRecipients) recipients.map(_.toAddress.toString).toSet else Set.empty
      val settings =
        UtxSettings(
          txs.length,
          PoolDefaultMaxBytes,
          1000,
          Set(sender.toAddress.toString),
          whitelist,
          allowTransactionsFromSmartAccounts = true,
          allowSkipChecks = false
        )
      val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
      (sender, utxPool, txs)
    }).label("massTransferWithBlacklisted")

  private def utxTest(utxSettings: UtxSettings, txCount: Int = 10)(f: (Seq[TransferTransaction], UtxPool, TestTime) => Unit): Unit =
    forAll(stateGen, chooseNum(2, txCount).label("txCount")) {
      case ((sender, senderBalance, bcu), count) =>
        val time = new TestTime()

        forAll(listOfN(count, transfer(sender, senderBalance / 2, time))) { txs =>
          val utx = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, utxSettings)
          f(txs, utx, time)
        }
    }

  private val dualTxGen: Gen[(UtxPool, TestTime, Seq[Transaction], Seq[Transaction])] =
    for {
      (sender, senderBalance, bcu) <- stateGen
      ts = System.currentTimeMillis()
      count1 <- chooseNum(5, 10)
      tx1    <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts)))
      tx2    <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts + maxAge.toMillis + 1000)))
    } yield {
      val time = new TestTime()
      val utx = new UtxPoolImpl(
        time,
        bcu,
        ignoreSpendableBalanceChanged,
        UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
      )
      (utx, time, tx1, tx2)
    }

  private val expr: EXPR = {
    val code =
      """let x = 1
        |let y = 2
        |true""".stripMargin
    ExpressionCompiler.compile(code, CompilerContext.empty).explicitGet()
  }

  private val script: Script = ExprScript(expr).explicitGet()

  private def preconditionBlocks(lastBlockId: ByteStr, master: KeyPair, time: Time): Seq[Block] = {
    val ts        = time.getTimestamp()
    val setScript = SetScriptTransaction.selfSigned(1.toByte, master, Some(script), 100000, ts + 1).explicitGet()
    Seq(TestBlock.create(ts + 1, lastBlockId, Seq(setScript)))
  }

  private def withScriptedAccount(scEnabled: Boolean): Gen[(KeyPair, Long, UtxPoolImpl, Long)] =
    for {
      (sender, senderBalance, bcu) <- stateGen
      time          = new TestTime()
      preconditions = preconditionBlocks(bcu.lastBlockId.get, sender, time)
    } yield {
      // val smartAccountsFs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))
      preconditions.foreach(b => bcu.processBlock(b) should beRight)
      val utx = new UtxPoolImpl(
        time,
        bcu,
        ignoreSpendableBalanceChanged,
        UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = scEnabled, allowSkipChecks = false)
      )

      (sender, senderBalance, utx, bcu.lastBlockTimestamp.getOrElse(0L))
    }

  private def transactionV1Gen(sender: KeyPair, ts: Long, feeAmount: Long): Gen[TransferTransaction] = accountGen.map { recipient =>
    TransferTransaction.selfSigned(1.toByte, sender, recipient.toAddress, Waves, waves(1), Waves, feeAmount, ByteStr.empty, ts).explicitGet()
  }

  private def transactionV2Gen(sender: KeyPair, ts: Long, feeAmount: Long): Gen[TransferTransaction] = accountGen.map { recipient =>
    TransferTransaction.selfSigned(2.toByte, sender, recipient.toAddress, Waves, waves(1), Waves, feeAmount, ByteStr.empty, ts).explicitGet()
  }

  "UTX Pool" - {
    "does not add new transactions when full" in utxTest(
      UtxSettings(1, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE should beRight
      all(txs.tail.map(t => utx.putIfNew(t).resultE)) should produce("pool size limit")
    }

    "does not add new transactions when full in bytes" in utxTest(
      UtxSettings(999999, 152, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE should beRight
      all(txs.tail.map(t => utx.putIfNew(t).resultE)) should produce("pool bytes size limit")
    }

    "adds new transactions when skip checks is allowed" in {
      forAll(stateGen) {
        case (sender, senderBalance, bcu) =>
          val time = new TestTime()

          val gen = for {
            headTransaction <- transfer(sender, senderBalance / 2, time)
            vipTransaction  <- transfer(sender, senderBalance / 2, time).suchThat(TransactionsOrdering.InUTXPool.compare(_, headTransaction) < 0)
          } yield (headTransaction, vipTransaction)

          forAll(gen, Gen.choose(0, 1).label("allowSkipChecks")) {
            case ((headTransaction, vipTransaction), allowSkipChecks) =>
              val utxSettings =
                UtxSettings(1, 152, 1, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = allowSkipChecks == 1)
              val utx = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, utxSettings)

              utx.putIfNew(headTransaction).resultE should beRight
              utx.putIfNew(vipTransaction).resultE should matchPattern {
                case Right(_) if allowSkipChecks == 1 =>
                case Left(_)                          =>
              }
          }
      }
    }

    "does not broadcast the same transaction twice if not allowed" in utxTest(
      utxSettings =
        UtxSettings(20, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE should matchPattern { case Right(true)  => }
      utx.putIfNew(txs.head).resultE should matchPattern { case Right(false) => }
    }

    "packUnconfirmed result is limited by constraint" in forAll(dualTxGen) {
      case (utx, _, txs, _) =>
        txs.foreach(tx => utx.putIfNew(tx).resultE should beRight)
        utx.all.size shouldEqual txs.size

        val maxNumber             = Math.max(utx.all.size / 2, 3)
        val rest                  = limitByNumber(maxNumber)
        val (packed, restUpdated) = utx.packUnconfirmed(rest, PackStrategy.Unlimited)

        packed.get.lengthCompare(maxNumber) should be <= 0
        if (maxNumber <= utx.all.size) restUpdated.isFull shouldBe true
    }

    "evicts expired transactions when packUnconfirmed is called" in forAll(dualTxGen) {
      case (utx, time, txs, _) =>
        txs.foreach(tx => utx.putIfNew(tx).resultE should beRight)
        utx.all.size shouldEqual txs.size

        time.advance(maxAge + 1000.millis)

        val (packed, _) = utx.packUnconfirmed(limitByNumber(100), PackStrategy.Unlimited)
        packed shouldBe empty
        utx.all shouldBe empty
    }

    "evicts one of mutually invalid transactions when packUnconfirmed is called" in forAll(twoOutOfManyValidPayments) {
      case (utx, time, txs, offset) =>
        txs.foreach(tx => utx.putIfNew(tx).resultE should beRight)
        utx.all.size shouldEqual txs.size

        time.advance(offset)

        val (packed, _) = utx.packUnconfirmed(limitByNumber(100), PackStrategy.Unlimited)
        packed.get.size shouldBe 2
        utx.all.size shouldBe 2
    }

    "correctly process constraints in packUnconfirmed" in {
      withDomain(wavesplatform.history.TransfersV2ActivatedAt0WavesSettings) { d =>
        val generateBlock: Gen[(KeyPair, Block, Seq[Transaction], Seq[Transaction])] =
          for {
            richAccount   <- accountGen
            randomAccount <- accountGen
          } yield {
            val genesisBlock = UnsafeBlocks.unsafeBlock(
              reference = randomSig,
              txs = Seq(
                GenesisTransaction.create(richAccount.toAddress, ENOUGH_AMT, ntpNow).explicitGet(),
                GenesisTransaction.create(randomAccount.toAddress, ENOUGH_AMT, ntpNow).explicitGet(),
                SetScriptTransaction
                  .signed(
                    1.toByte,
                    richAccount.publicKey,
                    Some(Script.fromBase64String("AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==").explicitGet()),
                    QuickTX.FeeAmount * 4,
                    ntpNow,
                    richAccount.privateKey
                  )
                  .explicitGet()
              ),
              signer = TestBlock.defaultSigner,
              version = 3,
              timestamp = ntpNow
            )
            val scripted   = (1 to 100).flatMap(_ => QuickTX.transferV2(richAccount, randomAccount.toAddress, timestamp = ntpTimestampGen).sample)
            val unscripted = (1 to 100).flatMap(_ => QuickTX.transfer(randomAccount, timestamp = ntpTimestampGen).sample)
            (richAccount, genesisBlock, scripted, unscripted)
          }

        val (_, block, scripted, unscripted) = generateBlock.sample.get
        d.blockchainUpdater.processBlock(block) should beRight

        val utx = new UtxPoolImpl(
          ntpTime,
          d.blockchainUpdater,
          ignoreSpendableBalanceChanged,
          UtxSettings(9999999, PoolDefaultMaxBytes, 999999, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
        )
        (scripted ++ unscripted).foreach(tx => utx.putIfNew(tx).resultE.explicitGet())

        val constraint = MultiDimensionalMiningConstraint(
          NonEmptyList.of(
            OneDimensionalMiningConstraint(1, TxEstimators.scriptRunNumber, "scriptRunNumber"),
            OneDimensionalMiningConstraint(Block.MaxTransactionsPerBlockVer3, TxEstimators.one, "KeyBlock")
          )
        )
        val (packed, _) = utx.packUnconfirmed(constraint, PackStrategy.Unlimited)
        packed.get.size shouldBe (unscripted.size + 1)
        packed.get.count(scripted.contains) shouldBe 1
      }
    }

    "pessimisticPortfolio" - {
      "is not empty if there are transactions" in forAll(withValidPayments) {
        case (sender, _, utxPool, _, _) =>
          utxPool.size should be > 0
          utxPool.pessimisticPortfolio(sender.toAddress) should not be empty
      }

      "is empty if there is no transactions" in forAll(emptyUtxPool) {
        case (sender, _, utxPool) =>
          utxPool.size shouldBe 0
          utxPool.pessimisticPortfolio(sender.toAddress) shouldBe empty
      }

      "is empty if utx pool was cleaned" in forAll(withValidPayments) {
        case (sender, _, utxPool, _, _) =>
          utxPool.removeAll(utxPool.all)
          utxPool.pessimisticPortfolio(sender.toAddress) shouldBe empty
      }

      "is changed after transactions with these assets are removed" in forAll(withValidPayments) {
        case (sender, _, utxPool, time, _) =>
          val portfolioBefore = utxPool.pessimisticPortfolio(sender.toAddress)
          val poolSizeBefore  = utxPool.size

          time.advance(maxAge * 2)
          utxPool.packUnconfirmed(limitByNumber(100), PackStrategy.Unlimited)

          poolSizeBefore should be > utxPool.size
          val portfolioAfter = utxPool.pessimisticPortfolio(sender.toAddress)

          portfolioAfter should not be portfolioBefore
      }
    }

    "spendableBalance" - {
      "equal to state's portfolio if utx is empty" in forAll(emptyUtxPool) {
        case (sender, _, utxPool) =>
          val pessimisticAssetIds = {
            val p = utxPool.pessimisticPortfolio(sender.toAddress)
            p.assetIds.filter(x => p.balanceOf(x) != 0)
          }

          pessimisticAssetIds shouldBe empty
      }

      "takes into account added txs" in forAll(withValidPaymentsNotAdded) {
        case (sender, _, utxPool, txs, _, _) =>
          val emptyPf = utxPool.pessimisticPortfolio(sender.toAddress)
          txs.foreach(utxPool.putIfNew(_).resultE should beRight)
          utxPool.pessimisticPortfolio(sender.toAddress) should not be emptyPf
      }

      "takes into account unconfirmed transactions" in pending
    }

    "blacklisting" - {
      "prevent a transfer transaction from specific addresses" in {
        val transferGen = Gen.oneOf(withBlacklisted, massTransferWithBlacklisted(allowRecipients = false))
        forAll(transferGen) {
          case (_, utxPool, txs) =>
            val r = txs.forall { tx =>
              utxPool.putIfNew(tx).resultE match {
                case Left(SenderIsBlacklisted(_)) => true
                case _                            => false
              }
            }

            r shouldBe true
            utxPool.all.size shouldEqual 0
        }
      }

      "allow a transfer transaction from blacklisted address to specific addresses" in {
        val transferGen = Gen.oneOf(withBlacklistedAndAllowedByRule, massTransferWithBlacklisted(allowRecipients = true))
        forAll(transferGen) {
          case (_, utxPool, txs) =>
            txs.foreach(utxPool.putIfNew(_).resultE should beRight)
            utxPool.all.size shouldEqual txs.size
        }
      }
    }

    "smart accounts" - {
      "signed txs from scripted account is not allowed" in {
        val enoughFeeTxWithScriptedAccount =
          for {
            (sender, senderBalance, utx, ts) <- withScriptedAccount(true)
            feeAmount                        <- choose(extraFee, senderBalance / 2)
            tx                               <- transactionV1Gen(sender, ts + 1, feeAmount)
          } yield (utx, tx)

        val (utx, tx) = enoughFeeTxWithScriptedAccount.sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
        utx.putIfNew(tx).resultE should produce("signature from scripted account")
      }

      "any transaction from scripted account is not allowed if smartAccounts disabled in utx pool" - {

        def enoughFeeTxWithScriptedAccount(version: Int): Gen[(UtxPoolImpl, TransferTransaction)] =
          for {
            (sender, senderBalance, utx, ts) <- withScriptedAccount(false)
            feeAmount                        <- choose(extraFee, senderBalance / 2)
            tx <- version match {
              case 1 => transactionV1Gen(sender, ts + 1, feeAmount)
              case 2 => transactionV2Gen(sender, ts + 1, feeAmount)
            }
          } yield (utx, tx)

        "v1" in {
          val (utx1, tx1) = enoughFeeTxWithScriptedAccount(1).sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
          utx1.putIfNew(tx1).resultE.left.value
        }
        "v2" in {
          val (utx2, tx2) = enoughFeeTxWithScriptedAccount(2).sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
          utx2.putIfNew(tx2).resultE should produce("denied from UTX pool")
        }
      }

      "when pack time limit is exceeded" - {
        "always packs the first transaction" in forAll(stateWithTransfer) {
          case (time, bcu, transfer) =>
            var timeSourceIsRunning = false
            def nanoTimeSource(): Long =
              if (timeSourceIsRunning) 100000L
              else {
                timeSourceIsRunning = true
                0L
              }
            val settings =
              UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
            val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings, nanoTimeSource = () => nanoTimeSource())

            utxPool.putIfNew(transfer).resultE should beRight
            val (tx, _) = utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Limit(100 nanos))
            tx.get should contain(transfer)
        }

        "retries until estimate" in withDomain() { d =>
          val settings =
            UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
          val utxPool     = new UtxPoolImpl(ntpTime, d.blockchainUpdater, ignoreSpendableBalanceChanged, settings)
          val startTime   = System.nanoTime()
          val (result, _) = utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Estimate(3 seconds))
          result shouldBe None
          (System.nanoTime() - startTime).nanos.toMillis shouldBe 3000L +- 1000
        }
      }
    }

    "cleanup" - {
      "doesnt validate transactions which are removed" in {
        val gen = for {
          acc  <- accountGen
          acc1 <- accountGen
          tx1  <- transfer(acc, ENOUGH_AMT / 3, ntpTime)
          txs  <- Gen.nonEmptyListOf(transfer(acc1, 10000000L, ntpTime).suchThat(_.fee < tx1.fee))
        } yield (tx1, txs)

        forAll(gen) {
          case (tx1, rest) =>
            val blockchain = stub[Blockchain]
            (() => blockchain.settings).when().returning(WavesSettings.default().blockchainSettings)
            (() => blockchain.height).when().returning(1)
            (() => blockchain.activatedFeatures).when().returning(Map.empty)

            val utx =
              new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
            (blockchain.balance _).when(*, *).returning(ENOUGH_AMT).repeat((rest.length + 1) * 2)

            (blockchain.balance _)
              .when(*, *)
              .onCall { (_: Address, _: Asset) =>
                utx.removeAll(rest)
                ENOUGH_AMT
              }
              .once()
            (blockchain.balance _).when(*, *).returning(ENOUGH_AMT)

            (blockchain.leaseBalance _).when(*).returning(LeaseBalance(0, 0))
            (blockchain.accountScript _).when(*).returns(None)
            val tb = TestBlock.create(Nil)
            (blockchain.blockHeader _).when(*).returning(Some(SignedBlockHeader(tb.header, tb.signature)))

            utx.putIfNew(tx1).resultE should beRight
            rest.foreach(utx.putIfNew(_).resultE should beRight)
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited) should matchPattern {
              case (Some(Seq(`tx1`)), _) => // Success
            }
            utx.all shouldBe Seq(tx1)
        }
      }
    }

    "priority pool" - {
      import TestValues.{script => testScript, scriptComplexity => testScriptComplexity}
      implicit class UtxPoolImplExt(utx: UtxPoolImpl) {
        def addPriorityTxs(txs: Seq[Transaction]): Unit = {
          val asDiffs = txs.map {
            case tt: TransferTransaction =>
              val pfs = Map(
                tt.sender.toAddress                -> -(tt.fee + tt.amount),
                tt.recipient.asInstanceOf[Address] -> tt.amount
              ).view.mapValues(Portfolio.waves).toMap
              Diff(tt, pfs)

            case tx => Diff(tx)
          }
          utx.addAndCleanupPriority(asDiffs)
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

        (blockchain.accountScript _).when(scripted).returns(Some(AccountScriptInfo(PublicKey(new Array[Byte](32)), testScript, testScriptComplexity, Map.empty)))
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
          utx.addPriorityTxs(minedTxs)

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

          val (left, right) = {
            val (left, right) = minedTxs.zipWithIndex.partition(kv => kv._2 % 2 == 0)
            (left.map(_._1), right.map(_._1))
          }

          utx.removeAll(left)
          val expectedTxs1 = right :+ tx1
          assertPortfolios(utx, expectedTxs1)
          all(right.map(utx.putIfNew(_).resultE)) shouldBe Right(false)
          utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(expectedTxs1)
          utx.all shouldBe expectedTxs1
          assertPortfolios(utx, expectedTxs1)

          val expectedTxs2 = expectedTxs1 ++ left.sorted(TransactionsOrdering.InUTXPool)
          utx.removeAll(expectedTxs2)
          left.foreach(utx.putIfNew(_).resultE should beRight)
          utx.addPriorityTxs(expectedTxs1)
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

          utx.addPriorityTxs(nonScripted)
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
          (blockchain.balance _).when(tx2.sender.toAddress, *).returning(ENOUGH_AMT).noMoreThanOnce() // initial validation
          (blockchain.balance _).when(tx2.sender.toAddress, *).returning(0)                           // Should be overriden in composite blockchain

          val utx =
            new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
          utx.addPriorityTxs(Seq(tx1))
          utx.putNewTx(tx2, false, false).resultE should beRight
          utx.nonPriorityTransactions shouldBe Seq(tx2)
          utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(tx1 :: tx2 :: Nil)
      }

      "runs cleanup on priority pool" in forAll(genDependent) {
        case (tx1, tx2) =>
          val blockchain = createState(tx1.sender.toAddress, setBalance = false)
          (blockchain.balance _).when(*, *).returning(0) // All invalid

          val utx =
            new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
          utx.addPriorityTxs(Seq(tx1, tx2))

          eventually(Timeout(5 seconds), Interval(50 millis))(utx.all shouldBe empty)
      }

      val genChain = for {
        acc <- accountGen
        genesis  = GenesisTransaction.create(acc.toAddress, ENOUGH_AMT, ntpTime.correctedTime()).explicitGet()
        genBlock = TestBlock.create(Seq(genesis))
        txs1 <- Gen.nonEmptyListOf(transferV2(acc, 10000000L, ntpTime))
        (block1, mbs1) = UnsafeBlocks.unsafeChainBaseAndMicro(
          genBlock.id(),
          Nil,
          Seq(Nil, txs1),
          acc,
          Block.NgBlockVersion,
          ntpTime.correctedTime()
        )
        txs2 <- Gen.nonEmptyListOf(transferV2(acc, 10000000L, ntpTime))
        txs4 <- Gen.nonEmptyListOf(transferV2(acc, 10000000L, ntpTime))
        (block2, mbs2) = UnsafeBlocks.unsafeChainBaseAndMicro(
          mbs1.head.totalResBlockSig,
          Nil,
          Seq(txs2, txs4),
          acc,
          Block.NgBlockVersion,
          ntpTime.correctedTime()
        )
        txs3 <- Gen.nonEmptyListOf(transferV2(acc, 10000000L, ntpTime))
        block3 = UnsafeBlocks.unsafeBlock(mbs2.last.totalResBlockSig, txs3, acc, Block.NgBlockVersion, ntpTime.correctedTime())
        block4 = UnsafeBlocks.unsafeBlock(genBlock.id(), txs4, acc, Block.NgBlockVersion, ntpTime.correctedTime())
      } yield (genBlock, (block1, mbs1), (block2, mbs2), block3, block4)

      val settingsWithNG = wavesplatform.history.settingsWithFeatures(BlockchainFeatures.NG, BlockchainFeatures.SmartAccounts)

      "applies chains correctly" in forAll(genChain) {
        case (genBlock, (block1, mbs1), (block2, mbs2), block3, block4) =>
          withDomain(settingsWithNG) { d =>
            implicit val scheduler: SchedulerService = Scheduler.singleThread("ext-appender")
            val blockchain                           = d.blockchainUpdater
            val utx =
              new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)

            val pos = stub[PoSSelector]
            (pos.validateBaseTarget _).when(*, *, *, *).returning(Right((): Unit))
            (pos.validateBlockDelay _).when(*, *, *, *).returning(Right((): Unit))
            (pos.validateGenerationSignature _).when(*).returning(Right(ByteStr(new Array[Byte](32))))

            val extAppender = ExtensionAppender(
              blockchain,
              utx,
              pos,
              ntpTime,
              stub[InvalidBlockStorage],
              stub[PeerDatabase],
              scheduler
            )(null, _)

            val microBlockAppender = MicroblockAppender(blockchain, utx, scheduler) _

            d.appendBlock(genBlock) shouldBe Nil
            d.appendBlock(block1) shouldBe Nil
            mbs1.foreach(microBlockAppender(_).runSyncUnsafe() should beRight)

            mbs2.head.transactionData.foreach(utx.putIfNew(_).resultE should beRight)
            extAppender(Seq(block2)).runSyncUnsafe() should beRight
            val expectedTxs1 = mbs1.last.transactionData ++ mbs2.head.transactionData.sorted(TransactionsOrdering.InUTXPool)
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(expectedTxs1)

            mbs2.foreach(microBlockAppender(_).runSyncUnsafe() should beRight)
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(mbs1.last.transactionData)

            extAppender(Seq(block3)).runSyncUnsafe() should beRight
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(mbs1.last.transactionData)

            // Not supported at the moment
            //extAppender(Seq(block4)).runSyncUnsafe() should beRight
            //val expectedTxs2 = mbs1.flatMap(_.transactionData) ++ mbs2.head.transactionData ++ block3.transactionData
            //utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(expectedTxs2)
            utx.close()
          }
      }

      "takes into account priority diff on putIfNew" in forAll(genDependent) {
        case (tx1, tx2) =>
          val blockchain = createState(tx1.sender.toAddress, setBalance = false)
          (blockchain.balance _).when(tx1.sender.toAddress, *).returning(ENOUGH_AMT)
          (blockchain.balance _).when(tx2.sender.toAddress, *).returning(ENOUGH_AMT).noMoreThanOnce()
          (blockchain.balance _).when(tx2.sender.toAddress, *).returning(0)

          val utx =
            new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
          utx.addPriorityTxs(Seq(tx1))
          utx.putNewTx(tx2, true, false).resultE.explicitGet()
          utx.nonPriorityTransactions shouldBe Seq(tx2)
          utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)._1 shouldBe Some(tx1 :: tx2 :: Nil)
      }
    }

    "event stream" - {
      "fires events correctly" in {
        val preconditions = for {
          richAcc   <- accountGen
          secondAcc <- accountGen
          ts = System.currentTimeMillis()
          fee <- smallFeeGen
          genesis = GenesisTransaction.create(richAcc.toAddress, ENOUGH_AMT, ts).explicitGet()
          validTransfer = TransferTransaction
            .selfSigned(TxVersion.V1, richAcc, secondAcc.toAddress, Waves, 1, Waves, fee, ByteStr.empty, ts)
            .explicitGet()
          invalidTransfer = TransferTransaction
            .selfSigned(TxVersion.V1, secondAcc, richAcc.toAddress, Waves, 2, Waves, fee, ByteStr.empty, ts)
            .explicitGet()
        } yield (genesis, validTransfer, invalidTransfer)

        forAll(preconditions) {
          case (genesis, validTransfer, invalidTransfer) =>
            withDomain() { d =>
              d.appendBlock(TestBlock.create(Seq(genesis)))
              val time   = new TestTime()
              val events = new ListBuffer[UtxEvent]
              val utxPool =
                new UtxPoolImpl(time, d.blockchainUpdater, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings, events += _)

              def assertEvents(f: Seq[UtxEvent] => Unit): Unit = {
                val currentEvents = events.toVector
                f(currentEvents)
                events.clear()
              }

              def addUnverified(tx: Transaction): Unit = {
                utxPool.addTransaction(tx, false)
              }

              val differ = TransactionDiffer(d.blockchainUpdater.lastBlockTimestamp, System.currentTimeMillis(), verify = false)(
                d.blockchainUpdater,
                _: Transaction
              ).resultE.explicitGet()
              val validTransferDiff = differ(validTransfer)
              addUnverified(validTransfer)
              addUnverified(invalidTransfer)
              assertEvents {
                case UtxEvent.TxAdded(`validTransfer`, `validTransferDiff`) +: Nil => // Pass
              }

              utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)
              assertEvents {
                case UtxEvent.TxRemoved(`invalidTransfer`, Some(_)) +: Nil => // Pass
              }

              utxPool.removeAll(Seq(validTransfer))
              assertEvents {
                case UtxEvent.TxRemoved(`validTransfer`, None) +: Nil => // Pass
              }

              addUnverified(validTransfer)
              events.clear()
              time.advance(maxAge + 1000.millis)
              utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, PackStrategy.Unlimited)
              assertEvents {
                case UtxEvent.TxRemoved(`validTransfer`, Some(GenericError("Expired"))) +: Nil => // Pass
              }
            }
        }
      }
    }
  }

  private def limitByNumber(n: Int): MultiDimensionalMiningConstraint = MultiDimensionalMiningConstraint(
    OneDimensionalMiningConstraint(n, TxEstimators.one, "one"),
    OneDimensionalMiningConstraint(n, TxEstimators.one, "one")
  )

}
