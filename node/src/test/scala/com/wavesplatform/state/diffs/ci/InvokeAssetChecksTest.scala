package com.wavesplatform.state.diffs.ci

import com.wavesplatform.TestValues
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.{V4, V5}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{Diff, InvokeScriptResult, NewTransactionInfo, Portfolio}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside}

import scala.collection.immutable.VectorMap

class InvokeAssetChecksTest extends PropSpec with Inside with WithState with DBCacheSettings with MockFactory with WithDomain with EitherValues {
  import DomainPresets._

  private val invalidLengthAsset = IssuedAsset(ByteStr.decodeBase58("WAVES").get)
  private val unexistingAsset    = IssuedAsset(ByteStr.decodeBase58("WAVESwavesWAVESwavesWAVESwavesWAVESwaves123").get)

  property("invoke asset checks") {
    val dApp = TestCompiler(V4).compileContract(
      s"""
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

    for {
      activated <- Seq(true, false)
      func      <- Seq("invalidLength", "unexisting")
    } {
      tempDb { _ =>
        val miner       = TxHelpers.signer(0).toAddress
        val invoker     = TxHelpers.signer(1)
        val master      = TxHelpers.signer(2)
        val genesis1Tx  = TxHelpers.genesis(master.toAddress)
        val genesis2Tx  = TxHelpers.genesis(invoker.toAddress)
        val setScriptTx = TxHelpers.setScript(master, dApp)
        val invoke      = TxHelpers.invoke(master.toAddress, Some(func), invoker = invoker)

        val dAppAddress = invoke.dAppAddressOrAlias.asInstanceOf[Address]
        def invokeInfo(succeeded: Boolean) =
          VectorMap(invoke.id() -> NewTransactionInfo(invoke, Set(invoke.senderAddress, dAppAddress), succeeded, if (!succeeded) 8L else 18L))

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
                invoke.senderAddress -> Portfolio(-invoke.fee),
                miner                -> Portfolio((setScriptTx.fee * 0.6 + invoke.fee * 0.4).toInt)
              ),
              scriptsComplexity = 8,
              scriptResults = Map(invoke.id() -> InvokeScriptResult(error = Some(ErrorMessage(1, expectingMessage))))
            )
          } else {
            val asset = if (func == "invalidLength") invalidLengthAsset else unexistingAsset
            Diff.empty.copy(
              transactions = invokeInfo(true),
              portfolios = Map(
                invoke.senderAddress -> Portfolio(-invoke.fee, assets = Map(asset -> 0)),
                dAppAddress          -> Portfolio.build(asset, 0),
                miner                -> Portfolio((setScriptTx.fee * 0.6 + invoke.fee * 0.4).toInt)
              ),
              scriptsRun = 1,
              scriptsComplexity = 18,
              scriptResults = Map(
                invoke.id() -> InvokeScriptResult(
                  transfers = Seq(
                    InvokeScriptResult.Payment(invoke.senderAddress, Waves, 0),
                    InvokeScriptResult.Payment(invoke.senderAddress, asset, 0)
                  )
                )
              )
            )
          }

        withDomain(if (activated) RideV5 else RideV4) { d =>
          d.appendBlock(genesis1Tx, genesis2Tx, setScriptTx)
          d.appendBlock(invoke)
          d.liquidDiff shouldBe expectedResult
        }
      }
    }
  }

  property("sync invoke asset checks") {
    def dApp(callingDApp: Address) = TestCompiler(V5).compileContract(
      s"""
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
         |@Callable(i)
         |func default() = []
       """.stripMargin
    )

    val dAppAcc             = TxHelpers.signer(0)
    val emptyDAppAcc        = TxHelpers.signer(1)
    val genesis             = TxHelpers.genesis(dAppAcc.toAddress, ENOUGH_AMT)
    val genesis2            = TxHelpers.genesis(emptyDAppAcc.toAddress, ENOUGH_AMT)
    val setDApp             = TxHelpers.setScript(dAppAcc, dApp(emptyDAppAcc.toAddress))
    val setDApp2            = TxHelpers.setScript(emptyDAppAcc, emptyDApp)
    val invokeInvalidLength = TxHelpers.invoke(dAppAcc.toAddress, Some("invalidLength"))
    val invokeUnexisting    = TxHelpers.invoke(dAppAcc.toAddress, Some("unexisting"))

    withDomain(RideV5) { d =>
      d.appendBlock(genesis, genesis2, setDApp, setDApp2)
      (the[RuntimeException] thrownBy d.appendBlock(invokeInvalidLength)).getMessage should include(
        s"Transfer error: invalid asset ID '$invalidLengthAsset' length = 4 bytes, must be 32"
      )
      (the[RuntimeException] thrownBy d.appendBlock(invokeUnexisting)).getMessage should include(
        s"Transfer error: asset '$unexistingAsset' is not found on the blockchain"
      )

    }
  }
}
