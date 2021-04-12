package com.wavesplatform.state.diffs.ci
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.FunctionHeader
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

  private val fs = TestFunctionalitySettings.Enabled

  property("invoke asset checks") {
    val illegalLengthAsset = IssuedAsset(ByteStr.decodeBase58("WAVES").get)
    val unexistingAsset    = IssuedAsset(ByteStr.decodeBase58("WAVESwavesWAVESwavesWAVESwavesWAVESwaves123").get)

    val script = TestCompiler(V4).compileContract(
      s"""
         |{-# STDLIB_VERSION 4       #-}
         |{-# CONTENT_TYPE   DAPP    #-}
         |{-# SCRIPT_TYPE    ACCOUNT #-}
         |
         |@Callable(i)
         |func invalidLength() =
         |  [
         |    ScriptTransfer(i.caller, 0, unit),
         |    ScriptTransfer(i.caller, 0, base58'$illegalLengthAsset')
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
        setScriptTx = SetScriptTransaction.selfSigned(1.toByte, master, Some(script), fee, ts + 2).explicitGet()
        call        = Some(FUNCTION_CALL(FunctionHeader.User(func), Nil))
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
              s"Invalid transferring asset '$illegalLengthAsset' length = 4 bytes != 32"
            else
              s"Transferring asset '$unexistingAsset' is not found in the blockchain"
          Diff.empty.copy(
            transactions = invokeInfo(false),
            portfolios = Map(
              invoke.senderAddress -> Portfolio.waves(-invoke.fee),
              miner                -> Portfolio.waves(invoke.fee)
            ),
            scriptsComplexity = 18,
            scriptResults = Map(invoke.id.value() -> InvokeScriptResult(error = Some(ErrorMessage(1, expectingMessage))))
          )
        } else {
          val asset = if (func == "invalidLength") illegalLengthAsset else unexistingAsset
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

      val features =
        if (activated)
          fs.copy(
            preActivatedFeatures = fs.preActivatedFeatures ++ Map(
              BlockchainFeatures.BlockV5.id          -> 0,
              BlockchainFeatures.SynchronousCalls.id -> 0
            )
          )
        else
          fs.copy(
            preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
          )

      assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invoke)), features)(
        _ shouldBe Right(expectedResult)
      )
    }
  }
}
