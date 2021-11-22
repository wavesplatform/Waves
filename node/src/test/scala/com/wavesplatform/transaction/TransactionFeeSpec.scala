package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.{BlockchainFeatures => BF}
import com.wavesplatform.history.settingsWithFeatures
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.utils.Signed

class TransactionFeeSpec extends FreeSpec with WithDomain {
  "invoke script" - {
    "with transfer and payment" in withDomain(DomainPresets.RideV5) { d =>
      val dAppAccount = TxHelpers.defaultSigner
      val dAppScript = TxHelpers.script(
        """{-# STDLIB_VERSION 5 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |
          |@Callable(i)
          |func default() = {
          |  [
          |    ScriptTransfer(i.caller, 1, unit)
          |  ]
          |}""".stripMargin
      )

      d.appendBlock(TxHelpers.genesis(dAppAccount.toAddress, 10.waves))
      d.appendBlock(TxHelpers.setScript(dAppAccount, dAppScript))
      val invokeScript = TxHelpers.invoke(dAppAccount.toAddress, "default", payments = Seq(InvokeScriptTransaction.Payment(1, Waves)))
      d.commonApi.calculateWavesFee(invokeScript) shouldBe 0.005.waves // No additional fee for transfer&payment
    }

    "with issued assets" in withDomain(settingsWithFeatures(BF.BlockV5, BF.Ride4DApps, BF.SynchronousCalls)) { d =>
      val dAppSigner  = TxHelpers.defaultSigner
      val dAppAddress = dAppSigner.toAddress

      d.appendBlock(
        GenesisTransaction.create(dAppAddress, 10.waves, ntpTime.getTimestamp()).explicitGet(),
        SetScriptTransaction
          .selfSigned(
            2.toByte,
            dAppSigner,
            Some(TestCompiler(V5).compileContract("""{-# STDLIB_VERSION 5 #-}
                                                    |{-# SCRIPT_TYPE ACCOUNT #-}
                                                    |{-# CONTENT_TYPE DAPP #-}
                                                    |
                                                    |@Callable(i)
                                                    |func default() = {
                                                    |  [
                                                    |    Issue("name", "description", 1000, 4, true, unit, 0),
                                                    |    Issue("name", "description", 1000, 4, true, unit, 1)
                                                    |  ]
                                                    |}
                                                    |""".stripMargin)),
            0.01.waves,
            ntpTime.getTimestamp()
          )
          .explicitGet()
      )

      val invokeScript = Signed.invokeScript(2.toByte, dAppSigner, dAppAddress, None, Seq.empty, 0.005.waves, Asset.Waves, ntpTime.getTimestamp())

      d.commonApi.calculateWavesFee(invokeScript) shouldBe 0.005.waves // Should NOT take the issued assets additional fee into account
    }

    "with verifier" - {
      "with complexity <= 200" in withDomain(settingsWithFeatures(BF.BlockV5, BF.Ride4DApps, BF.SynchronousCalls)) { d =>
        val dappAccount = TxHelpers.signer(1)
        val proxy       = TxHelpers.signer(2)
        val sender      = TxHelpers.signer(3)

        d.appendBlock(
          GenesisTransaction.create(dappAccount.toAddress, 20.waves, ntpTime.getTimestamp()).explicitGet(),
          GenesisTransaction.create(proxy.toAddress, 20.waves, ntpTime.getTimestamp()).explicitGet(),
          GenesisTransaction.create(sender.toAddress, 20.waves, ntpTime.getTimestamp()).explicitGet()
        )

        d.appendBlock(
          SetScriptTransaction
            .selfSigned(
              2.toByte,
              sender,
              Some(TestCompiler(V5).compileContract("""{-# STDLIB_VERSION 5 #-}
                                                      |{-# SCRIPT_TYPE ACCOUNT #-}
                                                      |{-# CONTENT_TYPE DAPP #-}
                                                      |
                                                      |@Verifier(tx)
                                                      |func verify() = {
                                                      |  true
                                                      |}
                                                      |""".stripMargin)),
              0.01.waves,
              ntpTime.getTimestamp()
            )
            .explicitGet(),
          SetScriptTransaction
            .selfSigned(
              2.toByte,
              dappAccount,
              Some(TestCompiler(V5).compileContract(s"""{-# STDLIB_VERSION 5 #-}
                                                       |{-# CONTENT_TYPE DAPP #-}
                                                       |{-# SCRIPT_TYPE ACCOUNT #-}
                                                       |
                                                       |@Callable(i)
                                                       |func default() = {
                                                       |  strict test = ${(1 to 10)
                                                         .map(_ => "sigVerify(base58'', base58'', base58'')")
                                                         .mkString(" || ")}
                                                       |  (
                                                       |    [
                                                       |    ScriptTransfer(i.caller, 100, unit)],
                                                       |    throw("error")
                                                       |  )
                                                       |}
                                                       |
                                                       |@Callable(i)
                                                       |func test() = {
                                                       |  strict test = ${(1 to 10)
                                                         .map(_ => "sigVerify(base58'', base58'', base58'')")
                                                         .mkString(" || ")}
                                                       |  [ScriptTransfer(i.caller, 100, unit)]
                                                       |}""".stripMargin)),
              0.01.waves,
              ntpTime.getTimestamp()
            )
            .explicitGet()
        )

        d.blockchain.accountScript(sender.toAddress).get.verifierComplexity should be <= 200L

        val invoke =
          Signed.invokeScript(TxVersion.V2, sender, dappAccount.toAddress, None, Seq.empty, 0.009.waves, Asset.Waves, ntpTime.getTimestamp())
        d.commonApi.calculateWavesFee(invoke) shouldBe 0.005.waves
        d.appendAndAssertFailed(invoke)

        val invoke2 = Signed.invokeScript(
          TxVersion.V2,
          sender,
          dappAccount.toAddress,
          Some(TxHelpers.functionCall("test")),
          Seq(InvokeScriptTransaction.Payment(1, Waves)),
          0.005.waves,
          Asset.Waves,
          ntpTime.getTimestamp()
        )
        d.commonApi.calculateWavesFee(invoke2) shouldBe 0.005.waves
        d.appendAndAssertSucceed(invoke2)
      }

      "with complexity > 200" in withDomain(settingsWithFeatures(BF.BlockV5, BF.Ride4DApps, BF.SynchronousCalls)) { d =>
        val dappAccount = TxHelpers.signer(1)
        val proxy       = TxHelpers.signer(2)
        val sender      = TxHelpers.signer(3)

        d.appendBlock(
          GenesisTransaction.create(dappAccount.toAddress, 20.waves, ntpTime.getTimestamp()).explicitGet(),
          GenesisTransaction.create(proxy.toAddress, 20.waves, ntpTime.getTimestamp()).explicitGet(),
          GenesisTransaction.create(sender.toAddress, 20.waves, ntpTime.getTimestamp()).explicitGet()
        )

        d.appendBlock(
          SetScriptTransaction
            .selfSigned(
              2.toByte,
              sender,
              Some(TestCompiler(V5).compileContract("""{-# STDLIB_VERSION 5 #-}
                                                      |{-# SCRIPT_TYPE ACCOUNT #-}
                                                      |{-# CONTENT_TYPE DAPP #-}
                                                      |
                                                      |@Verifier(tx)
                                                      |func verify() = {
                                                      |  let p0 = if (sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)) then 1 else 0
                                                      |  let p1 = if (sigVerify(tx.bodyBytes, tx.proofs[1], tx.senderPublicKey)) then 1 else 0
                                                      |  let p2 = if (sigVerify(tx.bodyBytes, tx.proofs[2], tx.senderPublicKey)) then 1 else 0
                                                      |
                                                      |  p0 + p1 + p2 >= 1
                                                      |}
                                                      |""".stripMargin)),
              0.01.waves,
              ntpTime.getTimestamp()
            )
            .explicitGet(),
          SetScriptTransaction
            .selfSigned(
              2.toByte,
              dappAccount,
              Some(TestCompiler(V5).compileContract(s"""{-# STDLIB_VERSION 5 #-}
                                                       |{-# CONTENT_TYPE DAPP #-}
                                                       |{-# SCRIPT_TYPE ACCOUNT #-}
                                                       |
                                                       |@Callable(i)
                                                       |func default() = {
                                                       |  strict test = ${(1 to 10)
                                                         .map(_ => "sigVerify(base58'', base58'', base58'')")
                                                         .mkString(" || ")}
                                                       |  (
                                                       |    [
                                                       |    ScriptTransfer(i.caller, 100, unit)],
                                                       |    throw("error")
                                                       |  )
                                                       |}
                                                       |""".stripMargin)),
              0.01.waves,
              ntpTime.getTimestamp()
            )
            .explicitGet()
        )

        d.blockchain.accountScript(sender.toAddress).get.verifierComplexity should be > 200L

        val invoke = Signed.invokeScript(2.toByte, sender, dappAccount.toAddress, None, Seq.empty, 0.009.waves, Asset.Waves, ntpTime.getTimestamp())
        d.commonApi.calculateWavesFee(invoke) shouldBe 0.009.waves
        d.appendAndAssertFailed(invoke)
      }
    }
  }

  "transfer transaction" - {
    "with sponsored asset fee" in withDomain(DomainPresets.ScriptsAndSponsorship.withActivationPeriod(1)) { d =>
      val issue = TxHelpers.issue()
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress, 10.waves))
      d.appendBlock(issue)
      d.appendBlock(
        SponsorFeeTransaction.selfSigned(TxVersion.V1, TxHelpers.defaultSigner, issue.asset, Some(1L), 1.waves, ntpTime.getTimestamp()).explicitGet()
      )

      val transfer = TransferTransaction
        .selfSigned(TxVersion.V2, TxHelpers.defaultSigner, TxHelpers.secondAddress, Waves, 1, issue.asset, 1L, ByteStr.empty, ntpTime.getTimestamp())
        .explicitGet()
      d.commonApi.calculateFee(transfer) shouldBe ((issue.asset, 1L, 0.001.waves))
    }

    "smart asset with smart account" in withDomain(DomainPresets.RideV4) { d =>
      val verifier = TxHelpers.script(
        """{-# STDLIB_VERSION 4       #-}
          |{-# CONTENT_TYPE   DAPP    #-}
          |{-# SCRIPT_TYPE    ACCOUNT #-}
          |@Verifier(t)
          |func verify() = true""".stripMargin
      )

      val assetScript = TxHelpers.script(
        """{-# STDLIB_VERSION 4          #-}
          |{-# CONTENT_TYPE   EXPRESSION #-}
          |{-# SCRIPT_TYPE    ASSET      #-}
          |true""".stripMargin
      )

      val issue = TxHelpers.issue(script = assetScript)
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress, 10.waves))
      d.appendBlock(issue)
      d.appendBlock(TxHelpers.setScript(TxHelpers.defaultSigner, verifier))

      val transfer = TxHelpers.transfer(from = TxHelpers.defaultSigner, asset = issue.asset, version = TxVersion.V2)
      d.commonApi.calculateWavesFee(transfer) shouldBe 0.009.waves
    }

    "after SynchronousCalls activated" in withDomain(DomainPresets.RideV5) { d =>
      val defaultSigner    = TxHelpers.signer(0)
      val secondSigner     = TxHelpers.signer(1)
      val thirdSigner      = TxHelpers.signer(2)
      val defaultRecipient = TxHelpers.signer(3)

      d.appendBlock(
        GenesisTransaction.create(defaultSigner.toAddress, 10.waves, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(secondSigner.toAddress, 10.waves, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(thirdSigner.toAddress, 10.waves, ntpTime.getTimestamp()).explicitGet(),
        SetScriptTransaction
          .selfSigned(
            2.toByte,
            defaultSigner,
            Some(TestCompiler(V5).compileExpression("""{-# STDLIB_VERSION 5 #-}
                                                      |{-# CONTENT_TYPE EXPRESSION #-}
                                                      |{-# SCRIPT_TYPE ACCOUNT #-}
                                                      |
                                                      |sigVerify_16Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
                                                      |""".stripMargin)),
            0.01.waves,
            ntpTime.getTimestamp()
          )
          .explicitGet(),
        SetScriptTransaction
          .selfSigned(
            2.toByte,
            secondSigner,
            Some(TestCompiler(V5).compileExpression("""{-# STDLIB_VERSION 5 #-}
                                                      |{-# CONTENT_TYPE EXPRESSION #-}
                                                      |{-# SCRIPT_TYPE ACCOUNT #-}
                                                      |
                                                      |match tx {
                                                      |  case t: Order | SetScriptTransaction => false
                                                      |  case _ =>
                                                      |    let s0 = if (sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)) then 1 else 0
                                                      |    let s1 = if (sigVerify(tx.bodyBytes, tx.proofs[1], tx.senderPublicKey)) then 1 else 0
                                                      |    let s2 = if (sigVerify(tx.bodyBytes, tx.proofs[2], tx.senderPublicKey)) then 1 else 0
                                                      |    s0 + s1 + s2 > 1
                                                      |}
                                                      |""".stripMargin)),
            0.01.waves,
            ntpTime.getTimestamp()
          )
          .explicitGet()
      )

      d.commonApi.calculateWavesFee(TxHelpers.transfer(defaultSigner, defaultRecipient.toAddress)) shouldBe 0.001.waves
      d.commonApi.calculateWavesFee(TxHelpers.transfer(secondSigner, defaultRecipient.toAddress)) shouldBe 0.005.waves
      d.commonApi.calculateWavesFee(TxHelpers.transfer(thirdSigner, defaultRecipient.toAddress)) shouldBe 0.001.waves
    }
  }
}
