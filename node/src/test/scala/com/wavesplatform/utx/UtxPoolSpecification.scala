package com.wavesplatform.utx

import cats.data.NonEmptyList
import com.wavesplatform
import com.wavesplatform.*
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.database.{RDB, RocksDBWriter, TestStorageFactory}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.UtxEvent
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.history.{DefaultWavesSettings, randomSig, settingsWithFeatures}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.mining.*
import com.wavesplatform.settings.*
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.{invoke as _, *}
import com.wavesplatform.state.utils.TestRocksDB
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.TxValidationError.{GenericError, SenderIsBlacklisted}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Transaction, *}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool.PackStrategy
import org.scalacheck.Gen.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.concurrent.Eventually

import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*
import scala.util.{Random, Using}

private object UtxPoolSpecification {
  final case class TempDB(fs: FunctionalitySettings, dbSettings: DBSettings) extends AutoCloseable {
    val path: Path            = Files.createTempDirectory("rocksdb-test")
    val rdb                   = RDB.open(dbSettings.copy(directory = path.toAbsolutePath.toString))
    val writer: RocksDBWriter = TestRocksDB.withFunctionalitySettings(rdb, fs)

    override def close(): Unit = {
      rdb.close()
      TestHelpers.deleteRecursively(path)
    }
  }
}

class UtxPoolSpecification extends FreeSpec with MockFactory with BlocksTransactionsHelpers with WithDomain with EitherValues with Eventually {
  private val PoolDefaultMaxBytes = 50 * 1024 * 1024 // 50 MB

  import DomainPresets.*
  import FeeValidation.ScriptExtraFee as extraFee
  import FunctionalitySettings.TESTNET.maxTransactionTimeBackOffset as maxAge
  import UtxPoolSpecification.*

  private def withBlockchain[A](genAccounts: Map[Address, Long])(test: BlockchainUpdaterImpl => A): A = {
    val genesisSettings = TestHelpers.genesisSettings(genAccounts)
    val origSettings    = WavesSettings.default()
    val settings = origSettings.copy(
      blockchainSettings = BlockchainSettings(
        'T',
        FunctionalitySettings.TESTNET.copy(preActivatedFeatures =
          Map(
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

    Using.resource(TempDB(settings.blockchainSettings.functionalitySettings, settings.dbSettings)) { dbContext =>
      val (bcu, _) = TestStorageFactory(settings, dbContext.rdb, new TestTime, ignoreBlockchainUpdateTriggers)
      bcu.processBlock(
        Block
          .genesis(
            genesisSettings,
            bcu.isFeatureActivated(BlockchainFeatures.RideV6),
            bcu.isFeatureActivated(BlockchainFeatures.LightNode)
          )
          .explicitGet()
      ) should beRight
      test(bcu)
    }
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

  private def invokeScript(sender: KeyPair, dApp: Address, time: Time) =
    Gen.choose(500000L, 600000L).map { fee =>
      Signed.invokeScript(TxVersion.V1, sender, dApp, None, Seq.empty, fee, Waves, time.getTimestamp())
    }

  private def dAppSetScript(sender: KeyPair, time: Time) = {
    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |@Callable(i)
        |func default() = { WriteSet([DataEntry("0", true)]) }
        |""".stripMargin
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV1).explicitGet()._1
    SetScriptTransaction.selfSigned(TxVersion.V1, sender, Some(script), extraFee, time.getTimestamp()).explicitGet()
  }

  private def withState[A](test: (KeyPair, Long, BlockchainUpdaterImpl) => A): A = {
    val sender        = TxHelpers.signer(1)
    val senderBalance = ENOUGH_AMT
    withBlockchain(Map(sender.toAddress -> senderBalance)) { bcu =>
      test(sender, senderBalance, bcu)
    }
  }

  private def withStateWithTransfer[A](test: (TestTime, BlockchainUpdaterImpl, TransferTransaction) => A): A = {
    withState { case (sender, _, bcu) =>
      val time = new TestTime
      test(time, bcu, transfer(sender, time))
    }
  }

  private def withStateWithThreeAccounts[A](test: (((KeyPair, Long), (KeyPair, Long), (KeyPair, Long)), BlockchainUpdaterImpl) => A): A = {
    val sender1        = TxHelpers.signer(1)
    val senderBalance1 = ENOUGH_AMT
    val sender2        = TxHelpers.signer(2)
    val senderBalance2 = ENOUGH_AMT
    val sender3        = TxHelpers.signer(3)
    val senderBalance3 = ENOUGH_AMT

    withBlockchain(Map(sender1.toAddress -> senderBalance1, sender2.toAddress -> senderBalance2, sender3.toAddress -> senderBalance3)) { bcu =>
      test(((sender1, senderBalance1), (sender2, senderBalance2), (sender3, senderBalance3)), bcu)
    }
  }

  private def withTwoOutOfManyValidPayments[A](test: (UtxPoolImpl, TestTime, IndexedSeq[TransferTransaction], FiniteDuration) => A): A =
    withState { case (sender, senderBalance, bcu) =>
      val recipient = TxHelpers.signer(2)

      val time = TestTime()
      val utx =
        new UtxPoolImpl(
          time,
          bcu,
          UtxSettings(
            10,
            PoolDefaultMaxBytes,
            1000,
            Set.empty,
            Set.empty,
            Set.empty,
            allowTransactionsFromSmartAccounts = true,
            allowSkipChecks = false,
            forceValidateInCleanup = false,
            alwaysUnlimitedExecution = false
          ),
          Int.MaxValue,
          isMiningEnabled = true
        )
      val amountPart = (senderBalance - extraFee) / 2 - extraFee
      val txs = for (_ <- 1 to 10) yield createWavesTransfer(sender, recipient.toAddress, amountPart, extraFee, time.getTimestamp()).explicitGet()
      test(utx, time, txs, 2000.millis)
    }

  private def withBlacklisted[A](test: (KeyPair, UtxPoolImpl, Seq[TransferTransaction]) => A): A =
    withState { case (sender, _, bcu) =>
      val recipient = TxHelpers.signer(2)
      val time      = TestTime()
      val txs       = (1 to 10).map(_ => transferWithRecipient(sender, recipient.publicKey, time))
      val settings =
        UtxSettings(
          10,
          PoolDefaultMaxBytes,
          1000,
          Set(sender.toAddress.toString),
          Set.empty,
          Set.empty,
          allowTransactionsFromSmartAccounts = true,
          allowSkipChecks = false,
          forceValidateInCleanup = false,
          alwaysUnlimitedExecution = false
        )
      val utxPool = new UtxPoolImpl(time, bcu, settings, Int.MaxValue, isMiningEnabled = true)
      test(sender, utxPool, txs)
    }

  private def withMassTransferWithBlacklisted[A](allowRecipients: Boolean)(test: (KeyPair, UtxPoolImpl, Seq[MassTransferTransaction]) => A): A = {
    withState { case (sender, senderBalance, bcu) =>
      val recipients = (1 to 10).map(idx => TxHelpers.signer(1 + idx).publicKey)
      val time       = TestTime()
      // @TODO: Random transactions
      val txs = (1 to 10).map(_ => massTransferWithRecipients(sender, recipients, senderBalance / 10, time)) ++
        (if (!allowRecipients) Seq(massTransferWithRecipients(sender, Seq.empty, senderBalance / 10, time)) else Seq.empty)
      val whitelist: Set[String] = if (allowRecipients) recipients.map(_.toAddress.toString).toSet else Set.empty
      val settings =
        UtxSettings(
          txs.length,
          PoolDefaultMaxBytes,
          1000,
          Set(sender.toAddress.toString),
          whitelist,
          Set.empty,
          allowTransactionsFromSmartAccounts = true,
          allowSkipChecks = false,
          forceValidateInCleanup = false,
          alwaysUnlimitedExecution = false
        )
      val utxPool = new UtxPoolImpl(time, bcu, settings, Int.MaxValue, isMiningEnabled = true)
      test(sender, utxPool, txs)
    }
  }

  private def withBlacklistedAndAllowedByRule[A](test: (KeyPair, UtxPoolImpl, Seq[TransferTransaction]) => A): A =
    withState { case (sender, _, bcu) =>
      val recipient = TxHelpers.signer(2)
      val time      = TestTime()
      val txs       = (1 to 10).map(_ => transferWithRecipient(sender, recipient.publicKey, time))
      val settings =
        UtxSettings(
          txs.length,
          PoolDefaultMaxBytes,
          1000,
          Set(sender.toAddress.toString),
          Set(recipient.toAddress.toString),
          Set.empty,
          allowTransactionsFromSmartAccounts = true,
          allowSkipChecks = false,
          forceValidateInCleanup = false,
          alwaysUnlimitedExecution = false
        )
      val utxPool = new UtxPoolImpl(time, bcu, settings, Int.MaxValue, isMiningEnabled = true)
      test(sender, utxPool, txs)
    }

  private def withBlacklistedAndWhitelisted[A](test: (KeyPair, UtxPoolImpl, Seq[TransferTransaction]) => A): A = {
    withState { case (sender, _, bcu) =>
      val recipient = TxHelpers.signer(2)
      val time      = TestTime()
      val txs       = (1 to 10).map(_ => transferWithRecipient(sender, recipient.publicKey, time))
      val settings =
        UtxSettings(
          txs.length,
          PoolDefaultMaxBytes,
          1000,
          Set(sender.toAddress.toString),
          Set.empty,
          Set(sender.toAddress.toString),
          allowTransactionsFromSmartAccounts = true,
          allowSkipChecks = false,
          forceValidateInCleanup = false,
          alwaysUnlimitedExecution = false
        )
      val utxPool = new UtxPoolImpl(time, bcu, settings, Int.MaxValue, isMiningEnabled = true)
      test(sender, utxPool, txs)
    }
  }

  private def withDualTxs[A](test: (UtxPool, TestTime, Seq[Transaction], Seq[Transaction]) => A): A =
    withState { case (sender, _, bcu) =>
      val ts    = System.currentTimeMillis()
      val count = 5
      val txs1  = (1 to count).map(idx => transfer(sender, TestTime(ts + idx)))
      val txs2  = (1 to count).map(idx => transfer(sender, TestTime(ts + idx + maxAge.toMillis + 1000)))
      val time  = TestTime()
      val utx = new UtxPoolImpl(
        time,
        bcu,
        UtxSettings(
          10,
          PoolDefaultMaxBytes,
          1000,
          Set.empty,
          Set.empty,
          Set.empty,
          allowTransactionsFromSmartAccounts = true,
          allowSkipChecks = false,
          forceValidateInCleanup = false,
          alwaysUnlimitedExecution = false
        ),
        Int.MaxValue,
        isMiningEnabled = true
      )
      test(utx, time, txs1, txs2)
    }

  private def withScriptedAccount[A](scEnabled: Boolean)(test: (KeyPair, Long, UtxPoolImpl, Long) => A): A =
    withState { case (sender, senderBalance, bcu) =>
      val time          = TestTime()
      val preconditions = preconditionBlocks(bcu.lastBlockId.get, sender, time)
      preconditions.foreach(b => bcu.processBlock(b) should beRight)
      val utx = new UtxPoolImpl(
        time,
        bcu,
        UtxSettings(
          10,
          PoolDefaultMaxBytes,
          1000,
          Set.empty,
          Set.empty,
          Set.empty,
          allowTransactionsFromSmartAccounts = scEnabled,
          allowSkipChecks = false,
          forceValidateInCleanup = false,
          alwaysUnlimitedExecution = false
        ),
        Int.MaxValue,
        isMiningEnabled = true
      )

      test(sender, senderBalance, utx, bcu.lastBlockTimestamp.getOrElse(0L))
    }

  private def transfer(sender: KeyPair, time: Time) =
    TransferTransaction
      .selfSigned(1.toByte, sender, TxHelpers.address(2), Waves, 1, Waves, extraFee, ByteStr.empty, time.getTimestamp())
      .explicitGet()

  private def transferWithRecipient(sender: KeyPair, recipient: PublicKey, time: Time) =
    TransferTransaction
      .selfSigned(1.toByte, sender, recipient.toAddress, Waves, 1, Waves, extraFee, ByteStr.empty, time.getTimestamp())
      .explicitGet()

  private def massTransferWithRecipients(sender: KeyPair, recipients: Seq[PublicKey], maxAmount: Long, time: Time) = {
    val amount    = maxAmount / (recipients.size + 1)
    val transfers = recipients.map(r => ParsedTransfer(r.toAddress, TxNonNegativeAmount.unsafeFrom(amount)))
    val minFee    = FeeValidation.FeeConstants(TransactionType.Transfer) + FeeValidation.FeeConstants(TransactionType.MassTransfer) * transfers.size
    MassTransferTransaction
      .selfSigned(1.toByte, sender, Waves, transfers, minFee, time.getTimestamp(), ByteStr.empty)
      .explicitGet()
  }

  private def transactionV1(sender: KeyPair, ts: Long, feeAmount: Long): TransferTransaction =
    TransferTransaction.selfSigned(1.toByte, sender, TxHelpers.address(2), Waves, waves(1), Waves, feeAmount, ByteStr.empty, ts).explicitGet()

  private def transactionV2(sender: KeyPair, ts: Long, feeAmount: Long): TransferTransaction =
    TransferTransaction.selfSigned(2.toByte, sender, TxHelpers.address(2), Waves, waves(1), Waves, feeAmount, ByteStr.empty, ts).explicitGet()

  private def utxTest(utxSettings: UtxSettings, txCount: Int = 10)(f: (Seq[TransferTransaction], UtxPool, TestTime) => Unit): Unit = {
    withState { case (sender, _, bcu) =>
      val time = TestTime()
      val txs  = (1 to txCount).map(_ => transfer(sender, time))

      val utx = new UtxPoolImpl(time, bcu, utxSettings, Int.MaxValue, isMiningEnabled = true)
      f(txs, utx, time)
    }
  }

  private def preconditionBlocks(lastBlockId: ByteStr, master: KeyPair, time: Time): Seq[Block] = {
    val ts = time.getTimestamp()
    val script = TestCompiler(V3).compileExpression(
      """
        |let x = 1
        |let y = 2
        |true
      """.stripMargin
    )
    val setScript = SetScriptTransaction.selfSigned(1.toByte, master, Some(script), 100000L, ts + 1).explicitGet()
    Seq(TestBlock.create(ts + 1, lastBlockId, Seq(setScript)).block)
  }

  "UTX Pool" - {
    "does not add new transactions when full" in utxTest(
      UtxSettings(
        1,
        PoolDefaultMaxBytes,
        1000,
        Set.empty,
        Set.empty,
        Set.empty,
        allowTransactionsFromSmartAccounts = true,
        allowSkipChecks = false,
        forceValidateInCleanup = false,
        alwaysUnlimitedExecution = false
      )
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE should beRight
      all(txs.tail.map(t => utx.putIfNew(t).resultE)) should produce("pool size limit")
    }

    "does not add new transactions when full in bytes" in utxTest(
      UtxSettings(
        999999,
        152,
        1000,
        Set.empty,
        Set.empty,
        Set.empty,
        allowTransactionsFromSmartAccounts = true,
        allowSkipChecks = false,
        forceValidateInCleanup = false,
        alwaysUnlimitedExecution = false
      )
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE should beRight
      all(txs.tail.map(t => utx.putIfNew(t).resultE)) should produce("pool bytes size limit")
    }

    "adds new transactions when skip checks is allowed" in {
      withState { case (sender, senderBalance, bcu) =>
        val time = TestTime()

        val gen = for {
          headTransaction <- transfer(sender, senderBalance / 2, time)
          vipTransaction <- transfer(sender, senderBalance / 2, time)
            .suchThat(TransactionsOrdering.InUTXPool(Set.empty).compare(_, headTransaction) < 0)
        } yield (headTransaction, vipTransaction)

        forAll(gen, Gen.choose(0, 1).label("allowSkipChecks")) { case ((headTransaction, vipTransaction), allowSkipChecks) =>
          val utxSettings =
            UtxSettings(
              1,
              152,
              1,
              Set.empty,
              Set.empty,
              Set.empty,
              allowTransactionsFromSmartAccounts = true,
              allowSkipChecks = allowSkipChecks == 1,
              forceValidateInCleanup = false,
              alwaysUnlimitedExecution = false
            )
          val utx = new UtxPoolImpl(time, bcu, utxSettings, Int.MaxValue, isMiningEnabled = true)

          utx.putIfNew(headTransaction).resultE should beRight
          utx.putIfNew(vipTransaction).resultE should matchPattern {
            case Right(_) if allowSkipChecks == 1 =>
            case Left(_)                          =>
          }
        }
      }
    }

    "adds new transactions when transaction is whitelisted" in {
      withStateWithThreeAccounts { case (((sender1, senderBalance1), (sender2, senderBalance2), (sender3, _)), bcu) =>
        val time = TestTime()

        val precondition = TestBlock
          .create(
            time.getTimestamp(),
            bcu.lastBlockId.get,
            Seq(dAppSetScript(sender3, time)),
            sender1
          )
          .block
        bcu.processBlock(precondition).explicitGet()

        val whiteListGen = Gen.oneOf(
          invokeScript(sender1, sender3.toAddress, time),
          transfer(sender2, senderBalance2 / 2, time)
        )

        val gen = for {
          headTransaction      <- transfer(sender1, senderBalance1 / 2, time)
          whitelistTransaction <- whiteListGen
        } yield (headTransaction, whitelistTransaction)

        forAll(gen, Arbitrary.arbBool.arbitrary.label("allowSkipChecks")) { case ((headTransaction, vipTransaction), allowSkipChecks) =>
          val utxSettings =
            UtxSettings(
              1,
              152,
              1,
              Set.empty,
              Set.empty,
              Set(sender2.toAddress.toString, sender3.toAddress.toString),
              allowTransactionsFromSmartAccounts = true,
              allowSkipChecks = allowSkipChecks,
              forceValidateInCleanup = false,
              alwaysUnlimitedExecution = false
            )
          val utx = new UtxPoolImpl(time, bcu, utxSettings, Int.MaxValue, isMiningEnabled = true)

          utx.putIfNew(headTransaction).resultE.explicitGet()
          utx.putIfNew(vipTransaction).resultE.explicitGet()
        }
      }
    }

    "does not broadcast the same transaction twice if not allowed" in utxTest(
      utxSettings = UtxSettings(
        20,
        PoolDefaultMaxBytes,
        1000,
        Set.empty,
        Set.empty,
        Set.empty,
        allowTransactionsFromSmartAccounts = true,
        allowSkipChecks = false,
        forceValidateInCleanup = false,
        alwaysUnlimitedExecution = false
      )
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE should matchPattern { case Right(true) => }
      utx.putIfNew(txs.head).resultE should matchPattern { case Right(false) => }
    }

    "packUnconfirmed result is limited by constraint" in withDualTxs { case (utx, _, txs, _) =>
      txs.foreach(tx => utx.putIfNew(tx).resultE should beRight)
      utx.all.size shouldEqual txs.size

      val maxNumber                = Math.max(utx.all.size / 2, 3)
      val rest                     = limitByNumber(maxNumber)
      val (packed, restUpdated, _) = utx.packUnconfirmed(rest, None, PackStrategy.Unlimited)

      packed.get.lengthCompare(maxNumber) should be <= 0
      if (maxNumber <= utx.all.size) restUpdated.isFull shouldBe true
    }

    "packUnconfirmed takes whitelisted first of all" in withStateWithThreeAccounts {
      case (((sender1, senderBalance1), (sender2, senderBalance2), (sender3, _)), bcu) =>
        val time = TestTime()

        val precondition = TestBlock
          .create(
            time.getTimestamp(),
            bcu.lastBlockId.get,
            Seq(dAppSetScript(sender3, time)),
            sender1
          )
          .block
        bcu.processBlock(precondition).explicitGet()

        val whiteListGen = Gen.listOfN(
          5,
          Gen.oneOf(
            invokeScript(sender1, sender3.toAddress, time),
            transfer(sender2, senderBalance2 / 20, time)
          )
        )

        val gen = Gen.listOfN(10, transfer(sender1, senderBalance1 / 20, time))

        forAll(whiteListGen, gen, Arbitrary.arbBool.arbitrary.label("allowSkipChecks")) { case (whitelistedTxs, txs, allowSkipChecks) =>
          val utxSettings =
            UtxSettings(
              10,
              1024 * 1000,
              1,
              Set.empty,
              Set.empty,
              Set(sender2.toAddress.toString, sender3.toAddress.toString),
              allowTransactionsFromSmartAccounts = true,
              allowSkipChecks = allowSkipChecks,
              forceValidateInCleanup = false,
              alwaysUnlimitedExecution = false
            )
          val utx = new UtxPoolImpl(time, bcu, utxSettings, Int.MaxValue, isMiningEnabled = true)

          Random.shuffle(whitelistedTxs ++ txs).foreach(tx => utx.putIfNew(tx))

          val (packed, _, _) = utx.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None, PackStrategy.Unlimited)
          packed.get.take(5) should contain theSameElementsAs whitelistedTxs
        }
    }

    "evicts expired transactions when packUnconfirmed is called" in withDualTxs { case (utx, time, txs, _) =>
      txs.foreach(tx => utx.putIfNew(tx).resultE should beRight)
      utx.all.size shouldEqual txs.size

      time.advance(maxAge + 1000.millis)

      val (packed, _, _) = utx.packUnconfirmed(limitByNumber(100), None, PackStrategy.Unlimited)
      packed shouldBe empty
      utx.all shouldBe empty
    }

    "evicts one of mutually invalid transactions when packUnconfirmed is called" in withTwoOutOfManyValidPayments { case (utx, time, txs, offset) =>
      txs.foreach(tx => utx.putIfNew(tx).resultE should beRight)
      utx.all.size shouldEqual txs.size

      time.advance(offset)

      val (packed, _, _) = utx.packUnconfirmed(limitByNumber(100), None, PackStrategy.Unlimited)
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
          UtxSettings(
            9999999,
            PoolDefaultMaxBytes,
            999999,
            Set.empty,
            Set.empty,
            Set.empty,
            allowTransactionsFromSmartAccounts = true,
            allowSkipChecks = false,
            forceValidateInCleanup = false,
            alwaysUnlimitedExecution = false
          ),
          Int.MaxValue,
          isMiningEnabled = true
        )
        (scripted ++ unscripted).foreach(tx => utx.putIfNew(tx).resultE.explicitGet())

        val constraint = MultiDimensionalMiningConstraint(
          NonEmptyList.of(
            OneDimensionalMiningConstraint(3, TxEstimators.scriptsComplexity, "KeyBlock"),
            OneDimensionalMiningConstraint(Block.MaxTransactionsPerBlockVer3, TxEstimators.one, "KeyBlock")
          )
        )
        val (packed, _, _) = utx.packUnconfirmed(constraint, None, PackStrategy.Unlimited)
        packed.get.size shouldBe (unscripted.size + 1)
        packed.get.count(scripted.contains) shouldBe 1
      }
    }

    "processes transaction fees" in {
      val blockMiner    = TxHelpers.signer(1200)
      val recipient     = TxHelpers.signer(1201)
      val initialAmount = 10000.waves
      val minerBalance  = initialAmount + 0.001.waves * 2

      withDomain(DomainPresets.NG, balances = Seq(AddrWithBalance(blockMiner.toAddress, minerBalance))) { d =>
        val transfer1 = TxHelpers.transfer(blockMiner, recipient.toAddress, version = 1.toByte, amount = initialAmount, fee = 0.001.waves)
        val transfer2 = TxHelpers.transfer(blockMiner, recipient.toAddress, version = 1.toByte, amount = 0.0004.waves, fee = 0.001.waves)
        d.appendBlock(
          d.createBlock(Block.NgBlockVersion, Seq.empty, generator = blockMiner)
        )
        d.utxPool.addTransaction(transfer1, verify = true)
        d.utxPool.addTransaction(transfer2, verify = true)

        d.utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None)._1.get shouldEqual Seq(transfer1, transfer2)
      }
    }

    "blacklisting" - {
      "prevent a transfer transaction from specific addresses" in {
        def test(utxPool: UtxPoolImpl, txs: Seq[Transaction]): Unit = {
          val r = txs.forall { tx =>
            utxPool.putIfNew(tx).resultE match {
              case Left(SenderIsBlacklisted(_)) => true
              case _                            => false
            }
          }

          r shouldBe true
          utxPool.all.size shouldEqual 0
          utxPool.close()
        }

        withBlacklisted { case (_, utxPool, txs) => test(utxPool, txs) }
        withMassTransferWithBlacklisted(allowRecipients = false) { case (_, utxPool, txs) => test(utxPool, txs) }
      }

      "allow a transfer transaction from blacklisted address to specific addresses" in {
        def test(utxPool: UtxPoolImpl, txs: Seq[Transaction]): Unit = {
          txs.foreach(utxPool.putIfNew(_).resultE should beRight)
          utxPool.all.size shouldEqual txs.size
          utxPool.close()
        }

        withBlacklistedAndAllowedByRule { case (_, utxPool, txs) => test(utxPool, txs) }
        withMassTransferWithBlacklisted(allowRecipients = true) { case (_, utxPool, txs) => test(utxPool, txs) }
      }

      "allow a transfer transaction from whitelisted address" in {
        withBlacklistedAndWhitelisted { case (_, utxPool, txs) =>
          all(txs.map { t =>
            utxPool.putIfNew(t).resultE
          }) shouldBe Symbol("right")
          utxPool.all.size shouldEqual txs.size
          utxPool.close()
        }
      }
    }

    "smart accounts" - {
      "signed txs from scripted account is not allowed" in {
        withScriptedAccount(scEnabled = true) { case (sender, _, utx, ts) =>
          val tx = transactionV1(sender, ts + 1, extraFee)

          utx.putIfNew(tx).resultE should produce("signature from scripted account")
        }
      }

      "any transaction from scripted account is not allowed if smartAccounts disabled in utx pool" - {

        def withVersion[A](version: Int)(test: (UtxPoolImpl, TransferTransaction) => A): A =
          withScriptedAccount(scEnabled = false) { case (sender, _, utx, ts) =>
            val tx = version match {
              case 1 => transactionV1(sender, ts + 1, extraFee)
              case 2 => transactionV2(sender, ts + 1, extraFee)
            }
            test(utx, tx)
          }

        "v1" in {
          withVersion(1) { case (utx, tx) =>
            utx.putIfNew(tx).resultE.left.value
          }
        }
        "v2" in {
          withVersion(2) { case (utx, tx) =>
            utx.putIfNew(tx).resultE should produce("denied from UTX pool")
          }
        }
      }

      "when pack time limit is exceeded" - {
        "always packs the first transaction" in withStateWithTransfer { case (time, bcu, transfer) =>
          var timeSourceIsRunning = false
          def nanoTimeSource(): Long =
            if (timeSourceIsRunning) 100000L
            else {
              timeSourceIsRunning = true
              0L
            }
          val settings =
            UtxSettings(
              10,
              PoolDefaultMaxBytes,
              1000,
              Set.empty,
              Set.empty,
              Set.empty,
              allowTransactionsFromSmartAccounts = true,
              allowSkipChecks = false,
              forceValidateInCleanup = false,
              alwaysUnlimitedExecution = false
            )
          val utxPool = new UtxPoolImpl(time, bcu, settings, Int.MaxValue, isMiningEnabled = true, nanoTimeSource = () => nanoTimeSource())

          utxPool.putIfNew(transfer).resultE should beRight
          val (tx, _, _) = utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None, PackStrategy.Limit(100 nanos))
          tx.get should contain(transfer)
          utxPool.close()
        }

        "retries until estimate" in withDomain() { d =>
          val settings =
            UtxSettings(
              10,
              PoolDefaultMaxBytes,
              1000,
              Set.empty,
              Set.empty,
              allowTransactionsFromSmartAccounts = true,
              allowSkipChecks = false,
              fastLaneAddresses = Set.empty,
              forceValidateInCleanup = false,
              alwaysUnlimitedExecution = false
            )
          val utxPool        = new UtxPoolImpl(ntpTime, d.blockchainUpdater, settings, Int.MaxValue, isMiningEnabled = true)
          val startTime      = System.nanoTime()
          val (result, _, _) = utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None, PackStrategy.Estimate(3 seconds))
          result shouldBe None
          (System.nanoTime() - startTime).nanos.toMillis shouldBe 3000L +- 1000
          utxPool.close()
        }
      }

      "dApp to dApp call chain in force validate mode" in withDomain(RideV5) { d =>
        def dApp(otherDApp: Option[Address]): Script =
          ScriptCompiler
            .compile(
              s"""
                 | {-# STDLIB_VERSION 5       #-}
                 | {-# CONTENT_TYPE   DAPP    #-}
                 | {-# SCRIPT_TYPE    ACCOUNT #-}
                 |
                 | @Callable(i)
                 | func default() = {
                 |    if (
                 |      let key = base64'LDCJzjgi5HtcHEXHfU8TZz+ZUHD2ZwsQ7JIEvzdMPYKYs9SoGkKUmg1yya4TE0Ms7x+KOJ4Ze/CPfKp2s5jbniFNM71N/YlHVbNkytLtQi1DzReSh9SNBsvskdY5mavQJe+67PuPVEYnx+lJ97qIG8243njZbGWPqUJ2Vqj49NAunhqX+eIkK3zAB3IPWls3gruzX2t9wrmyE9cVVvf1kgWx63PsQV37qdH0KcFRpCH89k4TPS6fLmqdFxX3YGHCGFTpr6tLogvjbUFJPT98kJ/xck0C0B/s8PTVKdao4VQHT4DBIO8+GB3CQVh6VV4EcMLtDWWNxF4yloAlKcFT0Q4AzJSimpFqd/SwSz9Pb7uk5srte3nwphVamC+fHlJt'
                 |      let proof = base64'GQPBoHuCPcIosF+WZKE5jZV13Ib4EdjLnABncpSHcMKBZl0LhllnPxcuzExIQwhxcfXvFFAjlnDGpKauQ9OQsjBKUBsdBZnGiV2Sg4TSdyHuLo2AbRRqJN0IV3iH3On8I4ngnL30ZAxVyGQH2EK58aUZGxMbbXGR9pQdh99QaiE='
                 |      let input = base64'IfZhAypdtgvecKDWzVyRuvXatmFf2ZYcMWVkCJ0/MQo='
                 |      bn256Groth16Verify_1inputs(key, proof, input)
                 |    ) then {
                 |      ${otherDApp.fold("")(address => s""" strict r = invoke(Address(base58'$address'), "default", [], []) """)}
                 |      []
                 |    } else {
                 |      throw("Error raised")
                 |    }
                 | }
               """.stripMargin,
              ScriptEstimatorV3(fixOverflow = true, overhead = true)
            )
            .explicitGet()
            ._1

        val (genesisTxs, setScripts) = (1 to 25).foldLeft((List.empty[GenesisTransaction], List.empty[SetScriptTransaction])) {
          case ((genesisTxs, setScripts), i) =>
            val account   = TxHelpers.signer(i)
            val script    = dApp(genesisTxs.headOption.map(_.recipient))
            val genesis   = TxHelpers.genesis(account.toAddress)
            val setScript = TxHelpers.setScript(account, script)
            (genesis :: genesisTxs, setScript :: setScripts)
        }

        d.appendBlock(genesisTxs*)
        d.appendBlock(setScripts*)

        val invoke = TxHelpers.invoke(genesisTxs.head.recipient, Some("default"))
        val utx = new UtxPoolImpl(
          ntpTime,
          d.blockchainUpdater,
          DefaultWavesSettings.utxSettings,
          DefaultWavesSettings.maxTxErrorLogSize,
          isMiningEnabled = true
        )
        utx.putIfNew(invoke, forceValidate = true).resultE.explicitGet() shouldBe true
        utx.removeAll(Seq(invoke))
        utx.putIfNew(invoke, forceValidate = false).resultE.explicitGet() shouldBe true
        utx.close()
      }

      "InvokeScriptTransaction is fully validated in forceValidate mode, on alwaysUnlimitedExecution = true and before 1000 complexity otherwise" in withDomain(
        RideV5,
        AddrWithBalance.enoughBalances(defaultSigner, secondSigner)
      ) { d =>
        def dApp(sigCount: Int) = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   strict c = ${(1 to sigCount).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
             |   if (true) then throw() else []
             | }
         """.stripMargin
        )
        val utx = new UtxPoolImpl(
          ntpTime,
          d.blockchainUpdater,
          DefaultWavesSettings.utxSettings,
          DefaultWavesSettings.maxTxErrorLogSize,
          isMiningEnabled = false
        )

        d.appendBlock(setScript(secondSigner, dApp(5)))
        utx.putIfNew(invoke()).resultE shouldBe Right(true)
        utx.putIfNew(invoke(), forceValidate = true).resultE should produce("Explicit script termination")

        val unlimitedUtx = utx.copy(utxSettings = DefaultWavesSettings.utxSettings.copy(alwaysUnlimitedExecution = true))
        unlimitedUtx.putIfNew(invoke()).resultE should produce("Explicit script termination")
        unlimitedUtx.putIfNew(invoke(), forceValidate = true).resultE should produce("Explicit script termination")

        d.appendBlock(setScript(secondSigner, dApp(4)))
        utx.putIfNew(invoke()).resultE should produce("Explicit script termination")
        utx.putIfNew(invoke(), forceValidate = true).resultE should produce("Explicit script termination")
        utx.close()
      }

      "correct events for InvokeScriptTransaction with big complexity on alwaysUnlimitedExecution = true" in withDomain(
        RideV5,
        AddrWithBalance.enoughBalances(defaultSigner, secondSigner)
      ) { d =>
        val recipient = signer(3).toAddress
        val dApp = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
             |   [ScriptTransfer(Address(base58'$recipient'), 1, unit)]
             | }
           """.stripMargin
        )
        val events = new ListBuffer[UtxEvent]
        val utx = new UtxPoolImpl(
          ntpTime,
          d.blockchainUpdater,
          DefaultWavesSettings.utxSettings.copy(alwaysUnlimitedExecution = true),
          DefaultWavesSettings.maxTxErrorLogSize,
          isMiningEnabled = false,
          events += _
        )
        d.appendBlock(setScript(secondSigner, dApp))

        val invokeTx = invoke()
        utx.putIfNew(invokeTx)
        val event = events.head.asInstanceOf[UtxEvent.TxAdded]
        event.tx.id() shouldBe invokeTx.id()
        event.snapshot.scriptsComplexity shouldBe 1011
        event.snapshot.balances((secondAddress, Waves)) shouldBe d.blockchainUpdater.balance(secondAddress, Waves) - 1
        event.snapshot.balances((recipient, Waves)) shouldBe d.blockchainUpdater.balance(recipient, Waves) + 1
        utx.close()
      }

      "sync calls are fully validated in forceValidate mode, on alwaysUnlimitedExecution = true and before 1000 complexity otherwise" in withDomain(
        RideV5,
        AddrWithBalance.enoughBalances(defaultSigner, secondSigner, signer(2))
      ) { d =>
        val innerDApp = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   strict r = Address(base58'$secondAddress').invoke("default", [], [])
             |   []
             | }
         """.stripMargin
        )
        def dApp(sigCount: Int) = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   strict c = ${(1 to sigCount).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
             |   if (true) then throw() else []
             | }
         """.stripMargin
        )
        val utx = new UtxPoolImpl(
          ntpTime,
          d.blockchainUpdater,
          DefaultWavesSettings.utxSettings,
          DefaultWavesSettings.maxTxErrorLogSize,
          isMiningEnabled = false
        )
        d.appendBlock(setScript(signer(2), innerDApp))

        d.appendBlock(setScript(secondSigner, dApp(5)))
        utx.putIfNew(invoke(signer(2).toAddress)).resultE shouldBe Right(true)
        utx.putIfNew(invoke(signer(2).toAddress), forceValidate = true).resultE should produce("Explicit script termination")

        val unlimitedUtx = utx.copy(utxSettings = DefaultWavesSettings.utxSettings.copy(alwaysUnlimitedExecution = true))
        unlimitedUtx.putIfNew(invoke(signer(2).toAddress)).resultE should produce("Explicit script termination")
        unlimitedUtx.putIfNew(invoke(signer(2).toAddress), forceValidate = true).resultE should produce("Explicit script termination")

        d.appendBlock(setScript(secondSigner, dApp(4)))
        utx.putIfNew(invoke(signer(2).toAddress)).resultE should produce("Explicit script termination")
        utx.putIfNew(invoke(signer(2).toAddress), forceValidate = true).resultE should produce("Explicit script termination")
        utx.close()
      }

      "invoke expression" in {
        val settings = settingsWithFeatures(
          BlockchainFeatures.Ride4DApps,
          BlockchainFeatures.BlockV5,
          BlockchainFeatures.RideV6,
          BlockchainFeatures.ContinuationTransaction
        )

        withDomain(settings.copy(featuresSettings = settings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))) { d =>
          d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, ENOUGH_AMT))

          val expr   = TestCompiler(V6).compileFreeCall(""" [ BooleanEntry("check", true) ] """)
          val invoke = TxHelpers.invokeExpression(expr)
          val utx    = new UtxPoolImpl(ntpTime, d.blockchainUpdater, DefaultWavesSettings.utxSettings, DefaultWavesSettings.maxTxErrorLogSize, true)

          utx.putIfNew(invoke).resultE.explicitGet() shouldBe true
          utx.all shouldBe Seq(invoke)

          val (result, _, _) = utx.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None, PackStrategy.Estimate(3 seconds))
          result shouldBe Some(Seq(invoke))
          utx.close()
        }
      }
    }

    "cleanup" - {
      "doesnt take the composite snapshot into account" in withDomain() { d =>
        d.helpers.creditWavesToDefaultSigner(11.waves)
        val transfers = Seq.fill(10)(TxHelpers.transfer(amount = 10.waves))
        transfers.foreach(tx => d.utxPool.addTransaction(tx, verify = false))
        d.utxPool.cleanUnconfirmed()
        d.utxPool.nonPriorityTransactions.toSet shouldBe transfers.toSet
      }

      "takes the priority diff into account" in withDomain() { d =>
        d.helpers.creditWavesToDefaultSigner(11.waves)
        val transfer1 = TxHelpers.transfer(amount = 10.waves)
        val transfer2 = TxHelpers.transfer(amount = 10.waves) // Double spend

        d.utxPool.priorityPool.setPriorityDiffs(Seq(d.createDiff(transfer1)))
        d.utxPool.addTransaction(transfer2, verify = false)

        d.utxPool.cleanUnconfirmed()
        d.utxPool.nonPriorityTransactions shouldBe Nil
      }

      "doesnt validate transactions which are removed" in {
        val gen = for {
          acc  <- accountGen
          acc1 <- accountGen
          tx1  <- transfer(acc, ENOUGH_AMT / 3, ntpTime)
          txs  <- Gen.nonEmptyListOf(transfer(acc1, 10000000L, ntpTime).suchThat(_.fee.value < tx1.fee.value))
        } yield (acc, acc1, tx1, txs)

        forAll(gen) { case (acc, acc1, tx1, rest) =>
          val blockchain = stub[Blockchain]
          (() => blockchain.settings).when().returning(WavesSettings.default().blockchainSettings)
          (() => blockchain.height).when().returning(1)
          (() => blockchain.activatedFeatures).when().returning(Map.empty)

          val utx =
            new UtxPoolImpl(
              ntpTime,
              blockchain,
              WavesSettings.default().utxSettings,
              WavesSettings.default().maxTxErrorLogSize,
              isMiningEnabled = true
            )
          (blockchain.balance _).when(*, *).returning(ENOUGH_AMT).repeat((rest.length + 1) * 2)

          (blockchain.balance _).when(*, *).returning(ENOUGH_AMT)

          (blockchain.wavesBalances _).when(*).returning(Map(acc.toAddress -> ENOUGH_AMT, acc1.toAddress -> ENOUGH_AMT))

          (blockchain.leaseBalance _).when(*).returning(LeaseBalance(0, 0))
          (blockchain.accountScript _).when(*).onCall { _: Address =>
            utx.removeAll(rest)
            None
          }
          val tb = TestBlock.create(Nil).block
          (blockchain.blockHeader _).when(*).returning(Some(SignedBlockHeader(tb.header, tb.signature)))

          utx.putIfNew(tx1).resultE should beRight
          rest.foreach(utx.putIfNew(_).resultE should beRight)
          utx.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None, PackStrategy.Unlimited) should matchPattern {
            case (Some(Seq(`tx1`)), _, _) => // Success
          }
          utx.all shouldBe Seq(tx1)
        }
      }

      "force validate transactions on non-miner node while cleanup when forceValidateInCleanup is turned on" in {
        checkCleanupForceValidateTxScenario(forceValidateInCleanup = true, isMiningEnabled = false)
      }

      "limited execution for transactions on non-miner node while cleanup when forceValidateInCleanup is turned off" in {
        checkCleanupForceValidateTxScenario(forceValidateInCleanup = false, isMiningEnabled = false)
      }

      "limited execution for transactions on miner node while cleanup" in {
        checkCleanupForceValidateTxScenario(forceValidateInCleanup = true, isMiningEnabled = true)
        checkCleanupForceValidateTxScenario(forceValidateInCleanup = false, isMiningEnabled = true)
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
            .selfSigned(TxVersion.V1, richAcc, secondAcc.toAddress, Waves, 1L, Waves, fee, ByteStr.empty, ts)
            .explicitGet()
          invalidTransfer = TransferTransaction
            .selfSigned(TxVersion.V1, secondAcc, richAcc.toAddress, Waves, 2L, Waves, fee, ByteStr.empty, ts)
            .explicitGet()
        } yield (genesis, validTransfer, invalidTransfer)

        forAll(preconditions) { case (genesis, validTransfer, invalidTransfer) =>
          withDomain() { d =>
            d.appendBlock(TestBlock.create(Seq(genesis)).block)
            val time   = TestTime()
            val events = new ListBuffer[UtxEvent]
            val utxPool =
              new UtxPoolImpl(
                time,
                d.blockchainUpdater,
                WavesSettings.default().utxSettings,
                WavesSettings.default().maxTxErrorLogSize,
                isMiningEnabled = true,
                events += _
              )

            def assertEvents(f: PartialFunction[Seq[UtxEvent], Unit]): Unit = {
              val currentEvents = events.toList
              f(currentEvents)
              events.clear()
            }

            def addUnverified(tx: Transaction): Unit = {
              utxPool.addTransaction(tx, verify = false)
            }

            val differ = TransactionDiffer(d.blockchainUpdater.lastBlockTimestamp, System.currentTimeMillis(), verify = false)(
              d.blockchainUpdater,
              _: Transaction
            ).resultE.explicitGet()
            val validTransferDiff = differ(validTransfer)
            addUnverified(validTransfer)
            addUnverified(invalidTransfer)
            assertEvents { case UtxEvent.TxAdded(`validTransfer`, `validTransferDiff`) +: Nil => // Pass
            }

            utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None, PackStrategy.Unlimited)
            assertEvents { case UtxEvent.TxRemoved(`invalidTransfer`, Some(_)) +: Nil => // Pass
            }

            utxPool.removeAll(Seq(validTransfer))
            assertEvents { case UtxEvent.TxRemoved(`validTransfer`, None) +: Nil => // Pass
            }

            addUnverified(validTransfer)
            events.clear()
            time.advance(maxAge + 1000.millis)
            utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.Unlimited, None, PackStrategy.Unlimited)
            assertEvents { case UtxEvent.TxRemoved(`validTransfer`, Some(GenericError("Expired"))) +: Nil => // Pass
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

  private def checkCleanupForceValidateTxScenario(forceValidateInCleanup: Boolean, isMiningEnabled: Boolean): Unit = {
    val dApp    = TxHelpers.signer(1)
    val invoker = TxHelpers.signer(2)

    val simpleContract: Script =
      TestCompiler(V5).compileContract(
        s"""
           |{-# STDLIB_VERSION 5 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func test() = {
           |  let flag = ${"sigVerify(base58'', base58'', base58'') ||" * 10} true
           |  (
           |    [ScriptTransfer(i.caller, -1, unit)],
           |    flag
           |  )
           |}
           |""".stripMargin
      )

    val selfInvokeContract =
      TestCompiler(V5).compileContract(
        s"""
           |{-# STDLIB_VERSION 5 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func test( r: Int ) = {
           |  if( r == 0 ) then [ ScriptTransfer(i.caller, -1, unit ) ] else
           |  let f = fraction( fraction( r, 1, 1 ), 1, 1 )
           |  strict g = invoke( this, "test", [ f - 1 ], [] )
           |  []
           |}
           |""".stripMargin
      )

    val settings = DomainPresets.RideV5.copy(
      utxSettings = DomainPresets.RideV5.utxSettings.copy(
        forceValidateInCleanup = forceValidateInCleanup
      ),
      minerSettings = DomainPresets.RideV5.minerSettings.copy(
        enable = isMiningEnabled
      )
    )

    Seq(
      (simpleContract, Seq.empty),
      (selfInvokeContract, Seq(CONST_LONG(9)))
    ).foreach { case (contract, args) =>
      withDomain(settings, balances = AddrWithBalance.enoughBalances(dApp, invoker)) { d =>
        val setScript = TxHelpers.setScript(dApp, contract)
        val invoke    = TxHelpers.invoke(dApp.toAddress, func = Some("test"), args = args, invoker = invoker)

        d.appendBlock(setScript)
        d.utxPool.addTransaction(invoke, verify = true)

        d.utxPool.size shouldBe 1

        d.utxPool.cleanUnconfirmed()

        val expectedResult = if (!isMiningEnabled && forceValidateInCleanup) {
          None
        } else {
          Some(invoke)
        }
        d.utxPool.transactionById(invoke.id()) shouldBe expectedResult
      }
    }
  }
}
