package com.wavesplatform.state.diffs.ci

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxVersion}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyncDAppComplexityCountTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory
    with EitherValues {

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> 0
    )
  )

  def dApp(otherDApp: Option[Address], paymentAsset: Option[IssuedAsset], transferAsset: Option[IssuedAsset], condition: String): Script = {
    val expr = {
      val script =
        s"""
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
           |      ${otherDApp.fold("")(address => s""" strict r = Invoke(Address(base58'$address'), "default", [], payment) """)}
           |      transfer
           |    } else {
           |      throw("Error raised")
           |    }
           | }
           """.stripMargin
      Parser.parseContract(script).get.value
    }
    ContractScript(V5, compileContractFromExpr(expr, V5)).explicitGet()
  }

  // ~1900 complexity
  val verifierScript: Script = {
    val script = s"""
                    | {-# STDLIB_VERSION 5        #-}
                    | {-# SCRIPT_TYPE ACCOUNT     #-}
                    | {-# CONTENT_TYPE EXPRESSION #-}
                    |
                    | let key = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
                    | let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
                    | let input = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs='
                    |
                    | groth16Verify_8inputs(key, proof, input)
                  """.stripMargin
    ScriptCompiler.compile(script, ScriptEstimatorV3).explicitGet()._1
  }

  // ~2700 complexity
  val groth: String =
    s"""
       | let key = base64'hwk883gUlTKCyXYA6XWZa8H9/xKIYZaJ0xEs0M5hQOMxiGpxocuX/8maSDmeCk3bo5ViaDBdO7ZBxAhLSe5k/5TFQyF5Lv7KN2tLKnwgoWMqB16OL8WdbePIwTCuPtJNAFKoTZylLDbSf02kckMcZQDPF9iGh+JC99Pio74vDpwTEjUx5tQ99gNQwxULtztsqDRsPnEvKvLmsxHt8LQVBkEBm2PBJFY+OXf1MNW021viDBpR10mX4WQ6zrsGL5L0GY4cwf4tlbh+Obit+LnN/SQTnREf8fPpdKZ1sa/ui3pGi8lMT6io4D7Ujlwx2RdCkBF+isfMf77HCEGsZANw0hSrO2FGg14Sl26xLAIohdaW8O7gEaag8JdVAZ3OVLd5Df1NkZBEr753Xb8WwaXsJjE7qxwINL1KdqA4+EiYW4edb7+a9bbBeOPtb67ZxmFqgyTNS/4obxahezNkjk00ytswsENg//Ee6dWBJZyLH+QGsaU2jO/W4WvRyZhmKKPdipOhiz4Rlrd2XYgsfHsfWf5v4GOTL+13ZB24dW1/m39n2woJ+v686fXbNW85XP/r'
       | let proof = base64'lvQLU/KqgFhsLkt/5C/scqs7nWR+eYtyPdWiLVBux9GblT4AhHYMdCgwQfSJcudvsgV6fXoK+DUSRgJ++Nqt+Wvb7GlYlHpxCysQhz26TTu8Nyo7zpmVPH92+UYmbvbQCSvX2BhWtvkfHmqDVjmSIQ4RUMfeveA1KZbSf999NE4qKK8Do+8oXcmTM4LZVmh1rlyqznIdFXPN7x3pD4E0gb6/y69xtWMChv9654FMg05bAdueKt9uA4BEcAbpkdHF'
       | let input = base64'LcMT3OOlkHLzJBKCKjjzzVMg+r+FVgd52LlhZPB4RFg='
       | groth16Verify(key, proof, input)
    """.stripMargin

  val assetScript: Script = {
    val script = s"""
                    | {-# STDLIB_VERSION 5        #-}
                    | {-# SCRIPT_TYPE ASSET       #-}
                    | {-# CONTENT_TYPE EXPRESSION #-}
                    |
                    | $groth
                    |
                  """.stripMargin
    ScriptCompiler.compile(script, ScriptEstimatorV3).explicitGet()._1
  }

  def scenario(
      dAppCount: Int,
      withPayment: Boolean,
      withThroughPayment: Boolean,
      withThroughTransfer: Boolean,
      withVerifier: Boolean,
      raiseError: Boolean
  ): Gen[(Seq[Transaction], InvokeScriptTransaction, IssuedAsset, Address)] =
    for {
      invoker  <- accountGen
      dAppAccs <- Gen.listOfN(dAppCount, accountGen)
      ts       <- timestampGen
      fee      <- ciFee(sc = (if (withVerifier) 1 else 0) + (if (withPayment) 1 else 0) + (if (withThroughTransfer) 1 else 0))
      assetIssue = IssueTransaction
        .selfSigned(
          2.toByte,
          invoker,
          "Payment asset",
          "",
          ENOUGH_AMT,
          8,
          reissuable = true,
          Some(assetScript),
          fee,
          ts + 1
        )
        .explicitGet()
      asset   = IssuedAsset(assetIssue.id.value())
      payment = List(Payment(1, asset))

      dAppGenesisTxs = dAppAccs.flatMap(
        a =>
          List(
            GenesisTransaction.create(a.toAddress, ENOUGH_AMT, ts).explicitGet(),
            TransferTransaction.selfSigned(TxVersion.V2, invoker, a.toAddress, asset, 10, Waves, fee, ByteStr.empty, ts).explicitGet()
          )
      )
      invokerGenesis = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()

      setScriptTxs = dAppAccs.foldLeft(List.empty[SetScriptTransaction]) {
        case (txs, currentAcc) =>
          val callPayment = if (withThroughPayment) Some(asset) else None
          val transfer    = if (withThroughTransfer) Some(asset) else None
          val condition   = if (raiseError) if (txs.nonEmpty) "true" else "false" else groth
          val callingDApp = Some(dApp(txs.headOption.map(_.sender.toAddress), callPayment, transfer, condition))
          val nextTx      = SetScriptTransaction.selfSigned(1.toByte, currentAcc, callingDApp, fee, ts + 5).explicitGet()
          nextTx :: txs
      }

      setVerifier = if (withVerifier)
        List(SetScriptTransaction.selfSigned(1.toByte, invoker, Some(verifierScript), fee, ts + 5).explicitGet())
      else
        Nil

      invokeTx = InvokeScriptTransaction
        .selfSigned(
          TxVersion.V3,
          invoker,
          dAppAccs.last.toAddress,
          None,
          if (withPayment) payment else Nil,
          fee,
          Waves,
          ts + 10
        )
        .explicitGet()
    } yield (
      Seq(invokerGenesis, assetIssue) ++ setVerifier ++ dAppGenesisTxs ++ setScriptTxs,
      invokeTx,
      asset,
      dAppAccs.head.toAddress
    )

  def assert(
      dAppCount: Int,
      complexity: Int,
      withPayment: Boolean = false,
      withThroughPayment: Boolean = false,
      withThroughTransfer: Boolean = false,
      withVerifier: Boolean = false,
      exceeding: Boolean = false,
      raiseError: Boolean = false,
      reject: Boolean = false
  ): Unit = {
    val (preparingTxs, invokeTx, asset, lastCallingDApp) =
      scenario(dAppCount, withPayment, withThroughPayment, withThroughTransfer, withVerifier, raiseError).sample.get
    assertDiffEi(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) { diffE =>
      if (reject) {
        diffE shouldBe Symbol("left")
        diffE should produce("Error raised")
      } else {
        val diff = diffE.explicitGet()
        diff.scriptsComplexity shouldBe complexity
        if (exceeding)
          diff.errorMessage(invokeTx.id.value()).get.text should include("Invoke complexity limit = 26000 is exceeded")
        else if (raiseError)
          diff.errorMessage(invokeTx.id.value()).get.text should include("Error raised")
        else
          diff.errorMessage(invokeTx.id.value()) shouldBe None

        val dAppAddress = invokeTx.dAppAddressOrAlias.asInstanceOf[Address]
        val basePortfolios = Map(
          TestBlock.defaultSigner.toAddress -> Portfolio(invokeTx.fee),
          invokeTx.senderAddress            -> Portfolio(-invokeTx.fee)
        )
        val paymentsPortfolios = Map(
          invokeTx.senderAddress -> Portfolio(assets = Map(asset -> -1)),
          dAppAddress            -> Portfolio(assets = Map(asset -> 1))
        )
        val throughTransfersPortfolios = Map(
          invokeTx.senderAddress -> Portfolio(assets = Map(asset -> 1)),
          lastCallingDApp        -> Portfolio(assets = Map(asset -> -1))
        )
        val throughPaymentsPortfolios =
          Map(lastCallingDApp -> Portfolio(assets = Map(asset -> 1))) |+|
            Map(dAppAddress   -> Portfolio(assets = Map(asset -> -1)))

        val overlappedPortfolio = Portfolio(assets = Map(asset -> 0))
        val emptyPortfolios     = Map.empty[Address, Portfolio]

        val additionalPortfolios =
          (if (withPayment) paymentsPortfolios else emptyPortfolios) |+|
            (if (withThroughPayment) throughPaymentsPortfolios else emptyPortfolios) |+|
            (if (withThroughTransfer) throughTransfersPortfolios else emptyPortfolios)

        val totalPortfolios = if (!exceeding) basePortfolios |+| additionalPortfolios else basePortfolios

        diff.portfolios.filter(_._2 != overlappedPortfolio) shouldBe totalPortfolios.filter(_._2 != overlappedPortfolio)
      }
    }
  }

  property("counts complexity correctly") {
    assert(1, 2709)
    assert(2, 5504)
    assert(18, 25041, exceeding = true)
    assert(19, 25041, exceeding = true)
    assert(100, 25041, exceeding = true)

    assert(1, 5415, withPayment = true)
    assert(2, 8210, withPayment = true)
    assert(17, 24965, withPayment = true, exceeding = true)
    assert(18, 24965, withPayment = true, exceeding = true)
    assert(100, 24965, withPayment = true, exceeding = true)

    assert(2, 8215, withThroughPayment = true)
    assert(9, 26000, withThroughPayment = true, exceeding = true)
    assert(10, 26000, withThroughPayment = true, exceeding = true)
    assert(100, 26000, withThroughPayment = true, exceeding = true)

    assert(1, 5424, withThroughTransfer = true)
    assert(2, 10934, withThroughTransfer = true)
    assert(9, 26000, withThroughTransfer = true, exceeding = true)
    assert(10, 25041, withThroughTransfer = true, exceeding = true)
    assert(100, 25041, withThroughTransfer = true, exceeding = true)

    assert(1, 4615, withVerifier = true)
    assert(17, 26947, withVerifier = true, exceeding = true)
    assert(18, 26947, withVerifier = true, exceeding = true)
    assert(19, 26947, withVerifier = true, exceeding = true)
    assert(100, 26947, withVerifier = true, exceeding = true)

    assert(1, 10036, withVerifier = true, withPayment = true, withThroughPayment = true, withThroughTransfer = true)
    assert(100, 26571, withVerifier = true, withPayment = true, withThroughPayment = true, withThroughTransfer = true, exceeding = true)
  }

  property("rejects and fails correctly") {
    assert(13, 0, raiseError = true, reject = true)
    assert(14, 1029, raiseError = true)
  }
}
