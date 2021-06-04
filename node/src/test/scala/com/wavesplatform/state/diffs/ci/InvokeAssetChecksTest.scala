package com.wavesplatform.state.diffs.ci
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{V4, V5}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{Diff, InvokeScriptResult, NewTransactionInfo, Portfolio}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InvokeAssetChecksTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory
    with WithDomain
    with EitherValues {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val invalidLengthAsset = IssuedAsset(ByteStr.decodeBase58("WAVES").get)
  private val unexistingAsset    = IssuedAsset(ByteStr.decodeBase58("WAVESwavesWAVESwavesWAVESwavesWAVESwaves123").get)

  property("invoke asset checks") {
    val dApp = TestCompiler(V4).compileContract(
      s"""
         |{-# STDLIB_VERSION 4       #-}
         |{-# CONTENT_TYPE   DAPP    #-}
         |{-# SCRIPT_TYPE    ACCOUNT #-}
         |
         |@Callable(i)
         |func invalidLength() =
         |  [
         |    ScriptTransfer(i.caller, 0, unit),
         |    ScriptTransfer(i.caller, 0, base58'$invalidLengthAsset')
         |  ]
         |
         |@Callable(i)
         |func unexisting() =
         |  [
         |    ScriptTransfer(i.caller, 0, unit),
         |    ScriptTransfer(i.caller, 0, base58'$unexistingAsset')
         |  ]
       """.stripMargin
    )

    val transferBase58WavesDAppScenario =
      for {
        activated <- Gen.oneOf(true, false)
        func      <- Gen.oneOf("invalidLength", "unexisting")
        master    <- accountGen
        invoker   <- accountGen
        fee       <- ciFee(nonNftIssue = 1)
        genesis1Tx  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2Tx  = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        setScriptTx = SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp), fee, ts + 2).explicitGet()
        call        = Some(FUNCTION_CALL(User(func), Nil))
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V2, invoker, master.toAddress, call, Seq(), fee, Waves, ts + 3)
          .explicitGet()
      } yield (activated, func, invokeTx, Seq(genesis1Tx, genesis2Tx, setScriptTx))

    val (activated, func, invoke, genesisTxs) = transferBase58WavesDAppScenario.sample.get
    tempDb { _ =>
      val miner       = TestBlock.defaultSigner.toAddress
      val dAppAddress = invoke.dAppAddressOrAlias.asInstanceOf[Address]
      def invokeInfo(succeeded: Boolean) =
        Map(invoke.id.value() -> NewTransactionInfo(invoke, Set(invoke.senderAddress, dAppAddress), succeeded))

      val expectedResult =
        if (activated) {
          val expectingMessage =
            if (func == "invalidLength")
              s"Transfer error: invalid asset ID '$invalidLengthAsset' length = 4 bytes, must be 32"
            else
              s"Transfer error: asset '$unexistingAsset' is not found on the blockchain"
          Diff.empty.copy(
            transactions = invokeInfo(false),
            portfolios = Map(
              invoke.senderAddress -> Portfolio.waves(-invoke.fee),
              miner                -> Portfolio.waves(invoke.fee)
            ),
            scriptsComplexity = 8,
            scriptResults = Map(invoke.id.value() -> InvokeScriptResult(error = Some(ErrorMessage(1, expectingMessage))))
          )
        } else {
          val asset = if (func == "invalidLength") invalidLengthAsset else unexistingAsset
          Diff.empty.copy(
            transactions = invokeInfo(true),
            portfolios = Map(
              invoke.senderAddress -> Portfolio(-invoke.fee, assets = Map(asset -> 0)),
              miner                -> Portfolio(invoke.fee),
              dAppAddress          -> Portfolio(-0, assets = Map(asset -> 0))
            ),
            scriptsRun = 1,
            scriptsComplexity = 18,
            scriptResults = Map(
              invoke.id.value() -> InvokeScriptResult(
                transfers = Seq(
                  InvokeScriptResult.Payment(invoke.senderAddress, Waves, 0),
                  InvokeScriptResult.Payment(invoke.senderAddress, asset, 0)
                )
              )
            )
          )
        }

      val fs = TestFunctionalitySettings.Enabled
      val features =
        if (activated)
          TestFunctionalitySettings.Enabled.copy(
            preActivatedFeatures = fs.preActivatedFeatures ++ Map(
              BlockchainFeatures.BlockV5.id          -> 0,
              BlockchainFeatures.SynchronousCalls.id -> 0
            )
          )
        else
          TestFunctionalitySettings.Enabled.copy(
            preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
          )

      assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke)), features)(
        _ shouldBe Right(expectedResult)
      )
    }
  }

  property("sync invoke asset checks") {
    def dApp(callingDApp: Address) = TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5       #-}
         |{-# CONTENT_TYPE   DAPP    #-}
         |{-# SCRIPT_TYPE    ACCOUNT #-}
         |
         |let callingDApp = Address(base58'$callingDApp')
         |
         |@Callable(i)
         |func invalidLength() = {
         |  strict r = invoke(callingDApp, "default", [], [AttachedPayment(base58'$invalidLengthAsset', 1)])
         |  []
         |}
         |
         |@Callable(i)
         |func unexisting() = {
         |  strict r = invoke(callingDApp, "default", [], [AttachedPayment(base58'$unexistingAsset', 1)])
         |  []
         |}
       """.stripMargin
    )

    val emptyDApp = TestCompiler(V5).compileContract(
      s"""
         |{-# STDLIB_VERSION 5       #-}
         |{-# CONTENT_TYPE   DAPP    #-}
         |{-# SCRIPT_TYPE    ACCOUNT #-}
         |
         |@Callable(i)
         |func default() = []
       """.stripMargin
    )

    val features = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.SmartAccounts.id    -> 0,
        BlockchainFeatures.SmartAssets.id      -> 0,
        BlockchainFeatures.Ride4DApps.id       -> 0,
        BlockchainFeatures.FeeSponsorship.id   -> 0,
        BlockchainFeatures.DataTransaction.id  -> 0,
        BlockchainFeatures.BlockReward.id      -> 0,
        BlockchainFeatures.BlockV5.id          -> 0,
        BlockchainFeatures.SynchronousCalls.id -> 0
      )
    )

    val preconditions =
      for {
        dAppAcc      <- accountGen
        emptyDAppAcc <- accountGen
        fee          <- ciFee()
        genesis  = GenesisTransaction.create(dAppAcc.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(emptyDAppAcc.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp  = SetScriptTransaction.selfSigned(1.toByte, dAppAcc, Some(dApp(emptyDAppAcc.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, emptyDAppAcc, Some(emptyDApp), fee, ts).explicitGet()
        invokeInvalidLength = InvokeScriptTransaction
          .selfSigned(1.toByte, dAppAcc, dAppAcc.toAddress, Some(FUNCTION_CALL(User("invalidLength"), Nil)), Nil, fee, Waves, ts)
          .explicitGet()
        invokeUnexisting = InvokeScriptTransaction
          .selfSigned(1.toByte, dAppAcc, dAppAcc.toAddress, Some(FUNCTION_CALL(User("unexisting"), Nil)), Nil, fee, Waves, ts)
          .explicitGet()
      } yield (List(genesis, genesis2, setDApp, setDApp2), invokeInvalidLength, invokeUnexisting)

    val (preparingTxs, invokeInvalidLength, invokeUnexisting) = preconditions.sample.get
    withDomain(domainSettingsWithFS(features)) { d =>
      d.appendBlock(preparingTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(invokeInvalidLength)).getMessage should include(
        s"Transfer error: invalid asset ID '$invalidLengthAsset' length = 4 bytes, must be 32"
      )
      (the[RuntimeException] thrownBy d.appendBlock(invokeUnexisting)).getMessage should include(
        s"Transfer error: asset '$unexistingAsset' is not found on the blockchain"
      )
    }
  }
}
