package com.wavesplatform.transaction

import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.{BlockchainFeatures => BF}
import com.wavesplatform.history.settingsWithFeatures
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}

class TransactionFeeSpec extends FreeSpec with WithDomain {
  "invoke with issued assets" in withDomain(settingsWithFeatures(BF.BlockV5, BF.Ride4DApps, BF.SynchronousCalls)) { d =>
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

    val invokeScript = InvokeScriptTransaction
      .selfSigned(2.toByte, dAppSigner, dAppAddress, None, Seq.empty, 0.005.waves, Asset.Waves, ntpTime.getTimestamp())
      .explicitGet()

    d.commonApi.calculateWavesFee(invokeScript) shouldBe 0.005.waves // Should NOT take the issued assets additional fee into account
  }

  "invoke with verifier" - {
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
                                                     |""".stripMargin)),
            0.01.waves,
            ntpTime.getTimestamp()
          )
          .explicitGet()
      )

      d.blockchain.accountScript(sender.toAddress).get.verifierComplexity should be <= 200L

      val invoke = InvokeScriptTransaction
        .selfSigned(2.toByte, sender, dappAccount.toAddress, None, Seq.empty, 0.009.waves, Asset.Waves, ntpTime.getTimestamp())
        .explicitGet()
      d.commonApi.calculateWavesFee(invoke) shouldBe 0.005.waves
      d.appendAndAssertFailed(invoke)
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

      val invoke = InvokeScriptTransaction
        .selfSigned(2.toByte, sender, dappAccount.toAddress, None, Seq.empty, 0.009.waves, Asset.Waves, ntpTime.getTimestamp())
        .explicitGet()
      d.commonApi.calculateWavesFee(invoke) shouldBe 0.009.waves
      d.appendAndAssertFailed(invoke)
    }
  }

  "transfer transaction" - {
    s"after ${BF.SynchronousCalls}" in withDomain(
      settingsWithFeatures(
        BF.SmartAccounts,
        BF.SmartAssets,
        BF.Ride4DApps,
        BF.FeeSponsorship,
        BF.DataTransaction,
        BF.BlockReward,
        BF.BlockV5,
        BF.SynchronousCalls
      )
    ) { d =>
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
