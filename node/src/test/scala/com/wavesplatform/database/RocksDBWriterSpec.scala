package com.wavesplatform.database

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.KeyTags.KeyTag
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.{V2, V5}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.{GenesisTransactionSettings, WavesSettings}
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.{TxHelpers, TxNonNegativeAmount, TxPositiveAmount}
import org.rocksdb.{ReadOptions, RocksIterator}

import scala.collection.mutable
import scala.util.{Random, Using}

class RocksDBWriterSpec extends FreeSpec with WithDomain {
  "Slice" - {
    "drops tail" in {
      RocksDBWriter.slice(Seq(10, 7, 4), 7, 10) shouldEqual Seq(10, 7)
    }
    "drops head" in {
      RocksDBWriter.slice(Seq(10, 7, 4), 4, 8) shouldEqual Seq(7, 4)
    }
    "includes Genesis" in {
      RocksDBWriter.slice(Seq(10, 7), 5, 11) shouldEqual Seq(10, 7, 1)
    }
  }
  "Merge" - {
    "correctly joins height ranges" in {
      RocksDBWriter.merge(Seq(15, 12, 3), Seq(12, 5)) shouldEqual Seq((15, 12), (12, 12), (3, 5))
      RocksDBWriter.merge(Seq(12, 5), Seq(15, 12, 3)) shouldEqual Seq((12, 15), (12, 12), (5, 3))
      RocksDBWriter.merge(Seq(8, 4), Seq(8, 4)) shouldEqual Seq((8, 8), (4, 4))
    }
  }

  val scriptOwner: KeyPair = TxHelpers.signer(1010)

  "caches are properly updated so that hasScript works as expected" in withDomain(
    DomainPresets.ScriptsAndSponsorship,
    Seq(AddrWithBalance(scriptOwner.toAddress, 100.waves))
  ) { d =>
    d.blockchain.hasAccountScript(scriptOwner.toAddress) shouldBe false

    val setScript = TxHelpers.setScript(
      scriptOwner,
      TestCompiler(V2).compileExpression("""{-# STDLIB_VERSION 2 #-}
                                           |{-# CONTENT_TYPE EXPRESSION #-}
                                           |{-# SCRIPT_TYPE ACCOUNT #-}
                                           |true""".stripMargin),
      0.01.waves
    )
    d.appendBlock(setScript)
    // check liquid block
    d.blockchain.hasAccountScript(scriptOwner.toAddress) shouldBe true
    d.appendBlock()
    // check if caches are updated when storing a block
    d.blockchain.hasAccountScript(scriptOwner.toAddress) shouldBe true

    // removing account script
    d.appendBlock(SetScriptTransaction.selfSigned(1.toByte, scriptOwner, None, 0.014.waves, ntpNow).explicitGet())
    d.blockchain.hasAccountScript(scriptOwner.toAddress) shouldBe false

    d.appendBlock()
    // check if caches are updated when storing a block
    d.blockchain.hasAccountScript(scriptOwner.toAddress) shouldBe false

    d.rollbackTo(3)
    // check if caches are updated during rollback
    d.blockchain.hasAccountScript(scriptOwner.toAddress) shouldBe true

    d.rollbackTo(1)
    // check if caches are updated during rollback
    d.blockchain.hasAccountScript(scriptOwner.toAddress) shouldBe false
  }

  private val settingsWithGenesis: WavesSettings = DomainPresets.NG.setFeaturesHeight(BlockchainFeatures.BlockReward -> 2)
  private val genesisBalance: Long               = 10 * 100.waves

  "wavesAmount includes genesis transactions" in withDomain(
    settingsWithGenesis.copy(blockchainSettings =
      settingsWithGenesis.blockchainSettings.copy(
        genesisSettings = settingsWithGenesis.blockchainSettings.genesisSettings.copy(
          initialBalance = genesisBalance,
          signature = None,
          transactions = (1 to 10).map(i => GenesisTransactionSettings(TxHelpers.address(1000 + 1).toString, 100.waves))
        )
      )
    )
  ) { d =>
    d.blockchain.wavesAmount(1) shouldBe genesisBalance
  }

  "readTransaction" - {
    val invoker = TxHelpers.signer(1002)
    val dapp    = TxHelpers.signer(1003)
    "reads correct failed transactions" in withDomain(
      DomainPresets.RideV5,
      Seq(AddrWithBalance(invoker.toAddress, 100.waves), AddrWithBalance(dapp.toAddress, 100.waves))
    ) { d =>
      val successfulInvoke = TxHelpers.invoke(dapp.toAddress, Some("foo"), Seq(CONST_BOOLEAN(true)), invoker = invoker)
      val failedInvoke     = TxHelpers.invoke(dapp.toAddress, Some("foo"), Seq(CONST_BOOLEAN(false)), invoker = invoker)
      d.appendBlock(
        TxHelpers.setScript(
          dapp,
          TestCompiler(V5).compileContract("""{-# STDLIB_VERSION 5 #-}
                                             |{-# CONTENT_TYPE DAPP #-}
                                             |{-# SCRIPT_TYPE ACCOUNT #-}
                                             |@Callable(i)
                                             |func foo(override: Boolean) = {
                                             |  if (sigVerify(base58'', base58'', base58'') ||
                                             |    sigVerify(base58'', base58'', base58'') ||
                                             |    sigVerify(base58'', base58'', base58'') ||
                                             |    sigVerify(base58'', base58'', base58'') ||
                                             |    sigVerify(base58'', base58'', base58'') ||
                                             |    override) then [] else throw("error")
                                             |}
                                             |""".stripMargin),
          fee = 0.01.waves
        ),
        successfulInvoke,
        failedInvoke
      )

      d.blockchain.transactionMeta(successfulInvoke.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
      d.blockchain.transactionMeta(failedInvoke.id()).map(_.status == Status.Succeeded) shouldBe Some(false)

      d.appendBlock()

      d.blockchain.transactionMeta(successfulInvoke.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
      d.blockchain.transactionMeta(failedInvoke.id()).map(_.status == Status.Succeeded) shouldBe Some(false)
    }
  }

  val aliasOwner: KeyPair = TxHelpers.signer(1001)
  "alias cache" in withDomain(DomainPresets.ScriptsAndSponsorship, Seq(AddrWithBalance(aliasOwner.toAddress, 200.waves))) { d =>
    val createAlias = TxHelpers.createAlias("foobar", aliasOwner, 0.001.waves)

    d.blockchain.resolveAlias(createAlias.alias) shouldEqual Left(AliasDoesNotExist(createAlias.alias))

    d.appendBlock(createAlias)
    // check liquid block
    d.blockchain.resolveAlias(createAlias.alias) shouldEqual Right(aliasOwner.toAddress)

    d.appendBlock()
    // check if caches are updated when storing a block
    d.blockchain.resolveAlias(createAlias.alias) shouldEqual Right(aliasOwner.toAddress)

    d.rollbackTo(1)
    // check if caches are updated after rollback
    d.blockchain.resolveAlias(createAlias.alias) shouldEqual Left(AliasDoesNotExist(createAlias.alias))
  }

  "deleteOldEntries" - {
    val settings = {
      val s = DomainPresets.RideV6
      s.copy(dbSettings = s.dbSettings.copy(maxRollbackDepth = 4, deleteOldDataInterval = 4))
    }

    val alice        = TxHelpers.signer(1)
    val aliceAddress = alice.toAddress

    val bob        = TxHelpers.signer(2)
    val bobAddress = bob.toAddress

    val carl        = TxHelpers.signer(3)
    val carlAddress = carl.toAddress

    val addresses = Seq(aliceAddress, bobAddress, carlAddress, TxHelpers.defaultSigner.toAddress)

    def transferWavesTx = TxHelpers.massTransfer(
      to = Seq(
        MassTransferTransaction.ParsedTransfer(aliceAddress, TxNonNegativeAmount.unsafeFrom(100.waves)),
        MassTransferTransaction.ParsedTransfer(bobAddress, TxNonNegativeAmount.unsafeFrom(100.waves))
      ),
      fee = 1.waves
    )

    val issueTx         = TxHelpers.issue(issuer = alice, amount = 100)
    def transferAssetTx = TxHelpers.transfer(from = alice, to = carlAddress, asset = issueTx.asset, amount = 1)

    val dataKey   = "test"
    val dataTxFee = TxPositiveAmount.unsafeFrom(TestValues.fee)
    def dataTx    = TxHelpers.dataSingle(account = bob, key = dataKey, value = Random.nextInt().toString, fee = dataTxFee.value)

    "doesn't delete sole data" in withDomain(settings, Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress))) { d =>
      d.appendBlock(transferWavesTx, issueTx, transferAssetTx, dataTx)

      (3 to 9).foreach(_ => d.appendBlock())
      d.blockchain.height shouldBe 9

      withClue("No data before current height: ") {
        checkHistoricalDataOnlySinceHeight(d, addresses, 2)
      }
    }

    "deletes old data and doesn't delete recent data" in withDomain(settings, Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress))) { d =>
      d.appendBlock(transferWavesTx, issueTx, transferAssetTx, dataTx)

      d.appendBlock()

      d.appendBlock(
        transferWavesTx,
        transferAssetTx,
        dataTx
      )
      d.blockchain.height shouldBe 4

      (5 to 9).foreach(_ => d.appendBlock())
      d.blockchain.height shouldBe 9

      withClue("No data before current height: ") {
        checkHistoricalDataOnlySinceHeight(d, addresses, 4)
      }
    }

    "deletes old data from previous intervals" in withDomain(settings, Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress))) { d =>
      (2 to 3).foreach(_ => d.appendBlock())

      d.appendBlock(
        transferWavesTx,
        issueTx,
        transferAssetTx,
        dataTx
      )
      d.blockchain.height shouldBe 4

      d.appendBlock()

      d.appendBlock(
        transferWavesTx,
        transferAssetTx,
        dataTx
      )
      d.blockchain.height shouldBe 6

      (7 to 13).foreach(_ => d.appendBlock())
      d.blockchain.height shouldBe 13

      withClue("No data before current height: ") {
        checkHistoricalDataOnlySinceHeight(d, addresses, 6)
      }
    }

    "doesn't affect other sequences" in {
      type CollectedKeys = mutable.ArrayBuffer[(ByteStr, String)]

      def collectNonHistoricalKeys(d: Domain): CollectedKeys = {
        val xs: CollectedKeys = mutable.ArrayBuffer.empty
        withGlobalIterator(d.rdb) { iter =>
          iter.seekToFirst()
          while (iter.isValid) {
            val k = iter.key()
            if (!(HistoricalKeyTags.exists(kt => k.startsWith(kt.prefixBytes)) || k.startsWith(KeyTags.HeightOf.prefixBytes))) {
              val description = KeyTags(Shorts.fromByteArray(k)).toString
              xs.addOne(ByteStr(k) -> description)
            }
            iter.next()
          }
        }
        xs
      }

      def appendBlocks(d: Domain): Unit = {
        (2 to 3).foreach(_ => d.appendBlock())
        d.appendBlock(transferWavesTx, issueTx, transferAssetTx, dataTx)

        d.appendBlock()
        d.appendBlock(transferWavesTx, transferAssetTx, dataTx)

        (7 to 13).foreach(_ => d.appendBlock())
      }

      var nonHistoricalKeysWithoutCleanup: CollectedKeys = mutable.ArrayBuffer.empty
      withDomain(
        settings.copy(dbSettings = settings.dbSettings.copy(deleteOldDataInterval = 1000)), // Won't delete old data
        Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress))
      ) { d =>
        appendBlocks(d)
        nonHistoricalKeysWithoutCleanup = collectNonHistoricalKeys(d)
      }

      withDomain(settings, Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress))) { d =>
        appendBlocks(d)
        val nonHistoricalKeys = collectNonHistoricalKeys(d)
        nonHistoricalKeys should contain theSameElementsInOrderAs nonHistoricalKeysWithoutCleanup
      }
    }
  }

  private val HistoricalKeyTags = Seq(
    KeyTags.ChangedAssetBalances,
    KeyTags.WavesBalanceHistory,
    KeyTags.AssetBalanceHistory,
    KeyTags.ChangedDataKeys,
    KeyTags.DataHistory,
    KeyTags.ChangedAddresses
  )

  private def checkHistoricalDataOnlySinceHeight(d: Domain, addresses: Seq[Address], sinceHeight: Int): Unit = {
    val addressIds = addresses.map(getAddressId(d, _))
    HistoricalKeyTags.foreach { keyTag =>
      withClue(s"$keyTag:") {
        d.rdb.db.iterateOver(keyTag) { e =>
          val (affectedHeight, affectedAddressIds) = getHeightAndAddressIds(keyTag, e)
          if (affectedAddressIds.exists(addressIds.contains)) {
            withClue(s"$addresses: ") {
              affectedHeight should be >= sinceHeight
            }
          }
        }
      }
    }
  }

  private def getHeightAndAddressIds(tag: KeyTag, bytes: DBEntry): (Int, Seq[AddressId]) = {
    val (heightBytes, addresses) = tag match {
      case KeyTags.ChangedAddresses | KeyTags.ChangedAssetBalances =>
        (
          bytes.getKey.drop(Shorts.BYTES),
          readAddressIds(bytes.getValue)
        )

      case KeyTags.WavesBalanceHistory | KeyTags.AssetBalanceHistory | KeyTags.ChangedDataKeys =>
        (
          bytes.getKey.takeRight(Ints.BYTES),
          Seq(AddressId.fromByteArray(bytes.getKey.dropRight(Ints.BYTES).takeRight(Longs.BYTES)))
        )

      case KeyTags.DataHistory =>
        (
          bytes.getKey.takeRight(Ints.BYTES),
          Seq(AddressId.fromByteArray(bytes.getKey.drop(Shorts.BYTES)))
        )

      case _ => throw new IllegalArgumentException(s"$tag")
    }

    (Ints.fromByteArray(heightBytes), addresses)
  }

  private def getAddressId(d: Domain, address: Address): AddressId =
    d.rdb.db.get(Keys.addressId(address)).getOrElse(throw new RuntimeException(s"Can't find address id for $address"))

  private def withGlobalIterator(rdb: RDB)(f: RocksIterator => Unit): Unit = {
    Using(new ReadOptions().setTotalOrderSeek(true)) { ro =>
      Using(rdb.db.newIterator(ro))(f).get
    }.get
  }
}
