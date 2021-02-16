package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
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
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
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

  property("Crosscontract call - multiple internal invokes - state update") {
    val groth =
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

    def dApp(otherDApp: Option[Address]): Script = {
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
             |      $groth
             |    ) then {
             |      ${otherDApp.fold("")(address => s""" strict r = Invoke(Address(base58'$address'), "default", [], []) """)}
             |      []
             |    } else {
             |      throw("unexpected")
             |    }
             | }
           """.stripMargin
        Parser.parseContract(script).get.value
      }
      ContractScript(V5, compileContractFromExpr(expr, V5)).explicitGet()
    }

    def scenario(dAppCount: Int) =
      for {
        invoker  <- accountGen
        dAppAccs <- Gen.listOfN(dAppCount, accountGen)
        ts       <- timestampGen
        fee      <- ciFee(dApps = dAppCount)

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

        dAppGenesisTxs = dAppAccs.map(a => GenesisTransaction.create(a.toAddress, ENOUGH_AMT, ts).explicitGet())
        invokerGenesis = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()

        setScriptTxs = dAppAccs.foldLeft(List.empty[SetScriptTransaction]) {
          case (txs, currentAcc) =>
            val callingDApp = Some(dApp(txs.headOption.map(_.sender.toAddress)))
            val nextTx      = SetScriptTransaction.selfSigned(1.toByte, currentAcc, callingDApp, fee, ts + 5).explicitGet()
            nextTx :: txs
        }
        payments = List(Payment(1, IssuedAsset(assetIssue.id.value())))
        invokeTx = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V3,
            invoker,
            dAppAccs.last.toAddress,
            None,
            Nil,
            fee,
            Waves,
            ts + 10
          )
          .explicitGet()
      } yield (
        dAppGenesisTxs ++ setScriptTxs ++ Seq(invokerGenesis /*, assetIssue*/ ),
        invokeTx
      )

    def assert(dAppCount: Int, complexity: Int, exceeding: Boolean): Unit = {
      val (preparingTxs, invokeTx) = scenario(dAppCount).sample.get
      assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
        case (diff, _) =>
          diff.portfolios shouldBe Map(
            TestBlock.defaultSigner.toAddress -> Portfolio.waves(invokeTx.fee),
            invokeTx.senderAddress            -> Portfolio.waves(-invokeTx.fee)
          )
          diff.scriptsComplexity shouldBe complexity
          if (exceeding)
            diff.errorMessage(invokeTx.id.value()).get.text should include("Invoke complexity limit = 52000 is exceeded")
          else
            diff.errorMessage(invokeTx.id.value()) shouldBe None
      }
    }

    assert(1, 2709, exceeding = false)
    assert(2, 5457, exceeding = false)
    assert(3, 8205, exceeding = false)
    assert(18, 49425, exceeding = false)
    assert(19, 52000, exceeding = true)
    assert(20, 51987, exceeding = true)
    assert(100, 51987, exceeding = true)
  }
}
