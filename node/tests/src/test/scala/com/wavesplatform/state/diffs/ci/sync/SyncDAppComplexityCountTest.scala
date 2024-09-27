package com.wavesplatform.state.diffs.ci.sync

import cats.instances.list.*
import cats.syntax.traverse.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.BlockDiffer.CurrentBlockFeePart
import com.wavesplatform.state.diffs.{ENOUGH_AMT, ci}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Transaction, TxHelpers}

class SyncDAppComplexityCountTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private def dApp(otherDApps: List[Address], paymentAsset: Option[IssuedAsset], transferAsset: Option[IssuedAsset], condition: String): Script =
    TestCompiler(V5).compileContract(s"""
                                        | {-# STDLIB_VERSION 5       #-}
                                        | {-# CONTENT_TYPE   DAPP    #-}
                                        | {-# SCRIPT_TYPE    ACCOUNT #-}
                                        |
                                        | @Callable(i)
                                        | func default() = {
                                        |    if (
                                        |      $condition
                                        |    ) then {
                                        |      let payment = ${paymentAsset.fold("[]")(id => s"[AttachedPayment(base58'$id', 1)]")}
                                        |      let transfer = ${transferAsset.fold("[]")(id => s"[ScriptTransfer(i.caller, 1, base58'$id')]")}
                                        |      ${otherDApps.mapWithIndex((a, i) => s""" strict r$i = invoke(Address(base58'$a'), "default", [], payment) """).mkString("\n")}
                                        |      transfer
                                        |    } else {
                                        |      throw("Error raised")
                                        |    }
                                        | }
                                        |""".stripMargin)

  // ~1900 complexity
  private val verifierScript: Script = {
    val script = s"""
                    | {-# STDLIB_VERSION 6        #-}
                    | {-# SCRIPT_TYPE ACCOUNT     #-}
                    | {-# CONTENT_TYPE EXPRESSION #-}
                    |
                    | let key = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
                    | let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
                    | let input = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs='
                    |
                    | groth16Verify_8inputs(key, proof, input)
                  """.stripMargin
    ScriptCompiler.compile(script, ScriptEstimatorV3.latest).explicitGet()._1
  }

  // ~2700 complexity
  private val groth: String =
    s"""
       | let key = base64'hwk883gUlTKCyXYA6XWZa8H9/xKIYZaJ0xEs0M5hQOMxiGpxocuX/8maSDmeCk3bo5ViaDBdO7ZBxAhLSe5k/5TFQyF5Lv7KN2tLKnwgoWMqB16OL8WdbePIwTCuPtJNAFKoTZylLDbSf02kckMcZQDPF9iGh+JC99Pio74vDpwTEjUx5tQ99gNQwxULtztsqDRsPnEvKvLmsxHt8LQVBkEBm2PBJFY+OXf1MNW021viDBpR10mX4WQ6zrsGL5L0GY4cwf4tlbh+Obit+LnN/SQTnREf8fPpdKZ1sa/ui3pGi8lMT6io4D7Ujlwx2RdCkBF+isfMf77HCEGsZANw0hSrO2FGg14Sl26xLAIohdaW8O7gEaag8JdVAZ3OVLd5Df1NkZBEr753Xb8WwaXsJjE7qxwINL1KdqA4+EiYW4edb7+a9bbBeOPtb67ZxmFqgyTNS/4obxahezNkjk00ytswsENg//Ee6dWBJZyLH+QGsaU2jO/W4WvRyZhmKKPdipOhiz4Rlrd2XYgsfHsfWf5v4GOTL+13ZB24dW1/m39n2woJ+v686fXbNW85XP/r'
       | let proof = base64'lvQLU/KqgFhsLkt/5C/scqs7nWR+eYtyPdWiLVBux9GblT4AhHYMdCgwQfSJcudvsgV6fXoK+DUSRgJ++Nqt+Wvb7GlYlHpxCysQhz26TTu8Nyo7zpmVPH92+UYmbvbQCSvX2BhWtvkfHmqDVjmSIQ4RUMfeveA1KZbSf999NE4qKK8Do+8oXcmTM4LZVmh1rlyqznIdFXPN7x3pD4E0gb6/y69xtWMChv9654FMg05bAdueKt9uA4BEcAbpkdHF'
       | let input = base64'LcMT3OOlkHLzJBKCKjjzzVMg+r+FVgd52LlhZPB4RFg='
       | groth16Verify(key, proof, input)
    """.stripMargin

  // ~70 complexity
  private val sigVerify: String =
    """ !sigVerify_32Kb(base58'', base58'', base58'') """

  private def assetScript(condition: String): Script = {
    val script = s"""
                    | {-# STDLIB_VERSION 5        #-}
                    | {-# SCRIPT_TYPE ASSET       #-}
                    | {-# CONTENT_TYPE EXPRESSION #-}
                    |
                    | $condition
                    |
                  """.stripMargin
    ScriptCompiler.compile(script, ScriptEstimatorV3.latest).explicitGet()._1
  }

  private def scenario(
      dAppCount: Int,
      withPayment: Boolean,
      withThroughPayment: Boolean,
      withThroughTransfer: Boolean,
      withVerifier: Boolean,
      raiseError: Boolean,
      sequentialCalls: Boolean,
      invokeExpression: Boolean
  ): (Seq[Transaction], InvokeTransaction, IssuedAsset, Address) = {
    val invoker  = TxHelpers.signer(0)
    val dAppAccs = (1 to dAppCount).map(idx => TxHelpers.signer(idx))

    val fee = TxHelpers.ciFee(
      sc = (if (withVerifier) 1 else 0) + (if (withPayment) 1 else 0) + (if (withThroughTransfer) 1 else 0),
      freeCall = invokeExpression
    )

    val invokerGenesis = TxHelpers.genesis(invoker.toAddress)
    val assetIssue     = TxHelpers.issue(invoker, ENOUGH_AMT, script = Some(assetScript(if (raiseError) sigVerify else groth)), fee = 1.waves)
    val asset          = IssuedAsset(assetIssue.id())
    val payment        = List(Payment(1, asset))

    val dAppGenesisTxs = dAppAccs.flatMap { dAppAcc =>
      List(
        TxHelpers.genesis(dAppAcc.toAddress),
        TxHelpers.transfer(invoker, dAppAcc.toAddress, 10, asset, fee = fee)
      )
    }
    val setScriptTxs = dAppAccs.foldLeft(List.empty[SetScriptTransaction]) { case (txs, currentAcc) =>
      val isLast      = txs.size == dAppAccs.size - 1
      val callPayment = if (withThroughPayment) Some(asset) else None
      val transfer    = if (withThroughTransfer) Some(asset) else None
      val condition   = if (raiseError) if (txs.nonEmpty) "true" else "false" else groth
      val script =
        if (sequentialCalls)
          if (isLast)
            dApp(txs.map(_.sender.toAddress), callPayment, transfer, condition)
          else
            dApp(Nil, callPayment, transfer, condition)
        else
          dApp(txs.headOption.map(_.sender.toAddress).toList, callPayment, transfer, condition)

      val nextTx = TxHelpers.setScript(currentAcc, script, fee)
      nextTx :: txs
    }

    val setVerifier = if (withVerifier) List(TxHelpers.setScript(invoker, verifierScript)) else Nil

    val invokeTx = TxHelpers.invoke(
      dApp = dAppAccs.last.toAddress,
      func = None,
      payments = if (withPayment) payment else Nil,
      invoker = invoker,
      fee = TxHelpers.ciFee(sc = (if (withVerifier) 1 else 0) + (if (withPayment) 1 else 0) + (if (withThroughTransfer) 1 else 0))
    )
    val invokeExpressionTx = ci.toInvokeExpression(setScriptTxs.head, invoker, Some(fee))

    (
      Seq(invokerGenesis, assetIssue) ++ setVerifier ++ dAppGenesisTxs ++ setScriptTxs,
      if (invokeExpression) invokeExpressionTx else invokeTx,
      asset,
      dAppAccs.head.toAddress
    )
  }

  private def assert(
      dAppCount: Int,
      complexity: Int,
      withPayment: Boolean = false,
      withThroughPayment: Boolean = false,
      withThroughTransfer: Boolean = false,
      withVerifier: Boolean = false,
      exceeding: Boolean = false,
      raiseError: Boolean = false,
      reject: Boolean = false,
      sequentialCalls: Boolean = false,
      invokeExpression: Boolean = false
  ): Unit = {
    val (preparingTxs, invokeTx, asset, lastCallingDApp) =
      scenario(dAppCount, withPayment, withThroughPayment, withThroughTransfer, withVerifier, raiseError, sequentialCalls, invokeExpression)
    withTestState(features(invokeExpression)) { (bu, db) =>
      assertDiffEi(
        Seq(TestBlock.create(preparingTxs), TestBlock.create(Seq.empty)),
        TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion),
        bu,
        db,
        enableExecutionLog = false
      ) { diffE =>
        if (reject) {
          diffE shouldBe Symbol("left")
          diffE should produce("Error raised")
        } else {
          val snapshot = diffE.explicitGet()
          snapshot.scriptsComplexity shouldBe complexity
          if (exceeding)
            snapshot.errorMessage(invokeTx.id()).get.text should include("Invoke complexity limit = 26000 is exceeded")
          else if (raiseError)
            snapshot.errorMessage(invokeTx.id()).get.text should include("Error raised")
          else
            snapshot.errorMessage(invokeTx.id()) shouldBe None

          val dAppAddress = invokeTx.dApp.asInstanceOf[Address]
          val basePortfolios =
            Map(TestBlock.defaultSigner.toAddress -> Portfolio(CurrentBlockFeePart(invokeTx.fee.value))) |+|
              Map(invokeTx.sender.toAddress       -> Portfolio(-invokeTx.fee.value))
          val paymentsPortfolios =
            Map(invokeTx.sender.toAddress -> Portfolio.build(asset, -1)) |+|
              Map(dAppAddress             -> Portfolio.build(asset, 1))
          val throughTransfersPortfolios =
            Map(invokeTx.sender.toAddress -> Portfolio.build(asset, 1)) |+|
              Map(lastCallingDApp         -> Portfolio.build(asset, -1))
          val throughPaymentsPortfolios =
            Map(lastCallingDApp -> Portfolio.build(asset, 1)) |+|
              Map(dAppAddress   -> Portfolio.build(asset, -1))

          val emptyPortfolios = Map.empty[Address, Portfolio]
          val additionalPortfolios =
            (if (withPayment) paymentsPortfolios else emptyPortfolios) |+|
              (if (withThroughPayment) throughPaymentsPortfolios else emptyPortfolios) |+|
              (if (withThroughTransfer) throughTransfersPortfolios else emptyPortfolios)

          val expectedPortfolios = if (exceeding || raiseError) basePortfolios else basePortfolios |+| additionalPortfolios
          expectedPortfolios
            .foreach { case (address, expectedPortfolio) =>
              expectedPortfolio.balance shouldBe snapshot.balances.get((address, Waves)).map(_ - db.balance(address)).getOrElse(0L)
              expectedPortfolio.assets.foreach { case (asset, balance) =>
                balance shouldBe snapshot.balances.get((address, asset)).map(_ - db.balance(address, asset)).getOrElse(0L)
              }
            }
        }
      }
    }
  }

  private implicit class Ops(m: Map[Address, Portfolio]) {
    def |+|(m2: Map[Address, Portfolio]): Map[Address, Portfolio] =
      Portfolio.combine(m, m2).explicitGet()
  }

  property("complexity border") {
    Seq(true, false).foreach { b =>
      assert(1, 2700, invokeExpression = b)
      assert(2, 5477, invokeExpression = b)
      assert(9, 24916, invokeExpression = b)
      assert(10, 24985, exceeding = true, invokeExpression = b)
      assert(100, 24985, exceeding = true, invokeExpression = b)

      assert(2, 8179, withThroughPayment = true, invokeExpression = b)
      assert(5, 24616, withThroughPayment = true, invokeExpression = b)
      assert(6, 24692, withThroughPayment = true, exceeding = true, invokeExpression = b)
      assert(100, 24692, withThroughPayment = true, exceeding = true, invokeExpression = b)

      assert(1, 4600, withVerifier = true, invokeExpression = b)
      assert(9, 26816, withVerifier = true, invokeExpression = b)
      assert(10, 26885, withVerifier = true, exceeding = true, invokeExpression = b)
      assert(100, 26885, withVerifier = true, exceeding = true, invokeExpression = b)
    }

    assert(1, 5400, withPayment = true)
    assert(2, 8177, withPayment = true)
    assert(8, 24839, withPayment = true)
    assert(9, 24909, withPayment = true, exceeding = true)
    assert(100, 24909, withPayment = true, exceeding = true)

    assert(1, 5402, withThroughTransfer = true)
    assert(2, 10881, withThroughTransfer = true)
    assert(4, 21839, withThroughTransfer = true)
    assert(5, 24618, withThroughTransfer = true, exceeding = true)
    assert(100, 24985, withThroughTransfer = true, exceeding = true)

    assert(1, 10002, withVerifier = true, withPayment = true, withThroughPayment = true, withThroughTransfer = true)
    assert(100, 26513, withVerifier = true, withPayment = true, withThroughPayment = true, withThroughTransfer = true, exceeding = true)
  }

  property("fail-free complexity border") {
    Seq(true, false).foreach { b =>
      assert(14, 0, raiseError = true, reject = true, invokeExpression = b)
      assert(15, 1065, raiseError = true, invokeExpression = b)

      assert(13, 0, raiseError = true, sequentialCalls = true, reject = true, invokeExpression = b)
      assert(14, 1013, raiseError = true, sequentialCalls = true, invokeExpression = b)

      assert(7, 0, raiseError = true, withThroughPayment = true, reject = true, invokeExpression = b)
      assert(8, 1044, raiseError = true, withThroughPayment = true, invokeExpression = b)

      assert(14, 0, raiseError = true, withThroughTransfer = true, reject = true, invokeExpression = b)
      assert(15, 1065, raiseError = true, withThroughTransfer = true, invokeExpression = b)
    }
  }

  private def features(invokeExpression: Boolean): FunctionalitySettings = {
    if (invokeExpression) {
      ContinuationTransaction.blockchainSettings.functionalitySettings
    } else {
      RideV6.blockchainSettings.functionalitySettings
    }
  }
}
