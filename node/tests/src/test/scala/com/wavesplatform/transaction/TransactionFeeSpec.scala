package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures as BF
import com.wavesplatform.history.settingsWithFeatures
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets._
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction

class TransactionFeeSpec extends FreeSpec with WithDomain {
  "invoke script" - {
    "with transfer and payment" in {
      val dAppAccount = TxHelpers.defaultSigner

      val balances = Seq(AddrWithBalance(dAppAccount.toAddress, 10.waves))

      withDomain(DomainPresets.RideV5, balances) { d =>
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

        d.appendBlock(TxHelpers.setScript(dAppAccount, dAppScript))
        val invokeScript = TxHelpers.invoke(dAppAccount.toAddress, Some("default"), payments = Seq(InvokeScriptTransaction.Payment(1, Waves)))
        d.commonApi.calculateWavesFee(invokeScript) shouldBe 0.005.waves // No additional fee for transfer&payment
      }
    }

    "with issued assets" in {
      val dAppSigner  = TxHelpers.defaultSigner
      val dAppAddress = dAppSigner.toAddress

      val balances = Seq(AddrWithBalance(dAppAddress, 10.waves))

      withDomain(settingsWithFeatures(BF.BlockV5, BF.Ride4DApps, BF.SynchronousCalls), balances) { d =>
        d.appendBlock(
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

        val invokeScript = TxHelpers.invoke(dAppAddress, invoker = dAppSigner, fee = 0.005.waves)

        d.commonApi.calculateWavesFee(invokeScript) shouldBe 0.005.waves // Should NOT take the issued assets additional fee into account
      }
    }

    "with verifier" - {
      "with complexity <= 200" in {
        val dappAccount = TxHelpers.signer(1)
        val proxy       = TxHelpers.signer(2)
        val sender      = TxHelpers.signer(3)

        val balances = Seq(dappAccount, proxy, sender).map(acc => AddrWithBalance(acc.toAddress, 20.waves))

        withDomain(settingsWithFeatures(BF.BlockV5, BF.Ride4DApps, BF.SynchronousCalls), balances) { d =>
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

          val invoke = TxHelpers.invoke(dappAccount.toAddress, invoker = sender, fee = 0.009.waves)
          d.commonApi.calculateWavesFee(invoke) shouldBe 0.005.waves
          d.appendAndAssertFailed(invoke)

          val invoke2 = TxHelpers.invoke(
            dApp = dappAccount.toAddress,
            func = Some("test"),
            payments = Seq(InvokeScriptTransaction.Payment(1, Waves)),
            invoker = sender,
            fee = 0.005.waves
          )
          d.commonApi.calculateWavesFee(invoke2) shouldBe 0.005.waves
          d.appendAndAssertSucceed(invoke2)
        }
      }

      "with complexity > 200" in {
        val dappAccount = TxHelpers.signer(1)
        val proxy       = TxHelpers.signer(2)
        val sender      = TxHelpers.signer(3)

        val balances = Seq(dappAccount, proxy, sender).map(acc => AddrWithBalance(acc.toAddress, 20.waves))

        withDomain(settingsWithFeatures(BF.BlockV5, BF.Ride4DApps, BF.SynchronousCalls), balances) { d =>
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

          val invoke = TxHelpers.invoke(dappAccount.toAddress, invoker = sender, fee = 0.009.waves)
          d.commonApi.calculateWavesFee(invoke) shouldBe 0.009.waves
          d.appendAndAssertFailed(invoke)
        }
      }
    }
  }

  "transfer transaction" - {
    "with sponsored asset fee" in {
      val balances = Seq(AddrWithBalance(TxHelpers.defaultAddress, 10.waves))

      withDomain(DomainPresets.ScriptsAndSponsorship.withActivationPeriod(1), balances) { d =>
        val issue = TxHelpers.issue()
        d.appendBlock(issue)
        d.appendBlock(
          SponsorFeeTransaction
            .selfSigned(TxVersion.V1, TxHelpers.defaultSigner, issue.asset, Some(1L), 1.waves, ntpTime.getTimestamp())
            .explicitGet()
        )

        val transfer = TransferTransaction
          .selfSigned(
            TxVersion.V2,
            TxHelpers.defaultSigner,
            TxHelpers.secondAddress,
            Waves,
            1,
            issue.asset,
            1L,
            ByteStr.empty,
            ntpTime.getTimestamp()
          )
          .explicitGet()
        d.commonApi.calculateFee(transfer) shouldBe ((issue.asset, 1L, 0.001.waves))
      }
    }

    "smart asset with smart account" in {
      val balances = Seq(AddrWithBalance(TxHelpers.defaultAddress, 10.waves))

      withDomain(DomainPresets.RideV4, balances) { d =>
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

        val issue = TxHelpers.issue(script = Some(assetScript))
        d.appendBlock(issue)
        d.appendBlock(TxHelpers.setScript(TxHelpers.defaultSigner, verifier))

        val transfer = TxHelpers.transfer(from = TxHelpers.defaultSigner, asset = issue.asset, version = TxVersion.V2)
        d.commonApi.calculateWavesFee(transfer) shouldBe 0.009.waves
      }
    }

    "after SynchronousCalls activated" in {
      val defaultSigner    = TxHelpers.signer(0)
      val secondSigner     = TxHelpers.signer(1)
      val thirdSigner      = TxHelpers.signer(2)
      val defaultRecipient = TxHelpers.signer(3)

      val balances = Seq(defaultSigner, secondSigner, thirdSigner).map(acc => AddrWithBalance(acc.toAddress, 10.waves))

      withDomain(DomainPresets.RideV5, balances) { d =>
        d.appendBlock(
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
}
