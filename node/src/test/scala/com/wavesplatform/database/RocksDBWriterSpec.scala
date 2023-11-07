package com.wavesplatform.database

import com.wavesplatform.TestValues
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
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
import com.wavesplatform.transaction.{TxHelpers, TxPositiveAmount}

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

  "deleteOldRecords" - {
    val maxRollbackDepth = 3
    val cleanupTestsSettings = {
      val s = DomainPresets.RideV6
      s.copy(dbSettings = s.dbSettings.copy(maxRollbackDepth = maxRollbackDepth))
    }

    val alice            = TxHelpers.signer(1)
    val aliceAddress     = alice.toAddress
    val aliceInitBalance = 100.waves

    val bob        = TxHelpers.signer(2)
    val bobAddress = bob.toAddress

    val carl        = TxHelpers.signer(3)
    val carlAddress = carl.toAddress

    val issueTx         = TxHelpers.issue(issuer = alice)
    def transferWavesTx = TxHelpers.transfer(from = alice, to = bobAddress)
    def transferAssetTx = TxHelpers.transfer(from = alice, to = carlAddress, asset = issueTx.asset, amount = 123)
    val dataKey         = "test"
    val dataTxFee       = TxPositiveAmount.unsafeFrom(TestValues.fee)

    def expectedValues(d: Domain): Unit = {
      val blocksSinceStart = d.blockchain.height - 2 // genesis and a block with issueTx

      markup("WAVES balance")
      d.balance(aliceAddress) shouldBe (aliceInitBalance - issueTx.fee.value - blocksSinceStart * List(
        transferWavesTx.amount,
        transferWavesTx.fee,
        transferAssetTx.fee,
        dataTxFee
      ).map(_.value).sum)
      d.balance(bobAddress) shouldBe (blocksSinceStart * transferWavesTx.amount.value)
      d.balance(carlAddress) shouldBe 0

      markup("Asset balance")
      d.balance(aliceAddress, issueTx.asset) shouldBe (issueTx.quantity.value - blocksSinceStart * transferAssetTx.amount.value)
      d.balance(bobAddress, issueTx.asset) shouldBe 0
      d.balance(carlAddress, issueTx.asset) shouldBe (blocksSinceStart * transferAssetTx.amount.value)

      markup("Data")
      if (blocksSinceStart > 0) d.blockchain.accountData(aliceAddress, dataKey).get.value shouldBe s"test-$blocksSinceStart"
      else d.blockchain.accountData(aliceAddress, dataKey) shouldBe empty
    }

    "doesn't affect current and past values" in withDomain(cleanupTestsSettings, Seq(AddrWithBalance(aliceAddress, aliceInitBalance))) { d =>
      d.appendBlock(issueTx)
      expectedValues(d)

      (1 to maxRollbackDepth + 1).foreach { i =>
        withClue(s"Append ${d.blockchain.height} block: ") {
          d.appendBlock(
            transferWavesTx,
            transferAssetTx,
            TxHelpers.dataSingle(account = alice, key = dataKey, value = s"test-$i", fee = dataTxFee.value)
          )
          expectedValues(d)
        }
      }

      (1 to maxRollbackDepth + 1).foreach { _ => // + 1 because of liquid block
        val rollbackHeight = d.blockchain.height - 1
        withClue(s"Rollback to $rollbackHeight:") {
          d.rollbackTo(rollbackHeight)
          expectedValues(d)
        }
      }
    }
  }
}
