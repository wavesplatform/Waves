package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.{V4, V5}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.{Diff, InvokeScriptResult, NewTransactionInfo, Portfolio}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{EitherValues, Inside}

import scala.collection.immutable.VectorMap

class InvokeAssetChecksTest extends PropSpec with Inside with WithState with DBCacheSettings with WithDomain with EitherValues {
  import DomainPresets.*

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
      func <- Seq("invalidLength", "unexisting")
    } {
      tempDb { _ =>
        val miner = TxHelpers.signer(0).toAddress
        val invoker = TxHelpers.signer(1)
        val master = TxHelpers.signer(2)
        val balances = AddrWithBalance.enoughBalances(invoker, master)
        val setScriptTx = TxHelpers.setScript(master, dApp)
        val invoke = TxHelpers.invoke(master.toAddress, Some(func), invoker = invoker)

        val dAppAddress = invoke.dApp.asInstanceOf[Address]

        def invokeInfo(succeeded: Boolean): VectorMap[ByteStr, NewTransactionInfo] =
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
                miner -> Portfolio((setScriptTx.fee * 0.6 + invoke.fee * 0.4).toLong + 6.waves)
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
                dAppAddress -> Portfolio.build(asset, 0),
                miner -> Portfolio((setScriptTx.fee * 0.6 + invoke.fee * 0.4).toLong + 6.waves)
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

        withDomain(if (activated) RideV5 else RideV4, balances) { d =>
          d.appendBlock(setScriptTx)
          d.appendBlock(invoke)
          d.liquidDiff shouldBe expectedResult
        }
      }
    }
  }

  property("sync invoke asset checks") {
    def dApp(callingDApp: Address): ContractScriptImpl = TestCompiler(V5).compileContract(
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
    val genesis             = TxHelpers.genesis(dAppAcc.toAddress)
    val genesis2            = TxHelpers.genesis(emptyDAppAcc.toAddress)
    val setDApp             = TxHelpers.setScript(dAppAcc, dApp(emptyDAppAcc.toAddress))
    val setDApp2            = TxHelpers.setScript(emptyDAppAcc, emptyDApp)
    val invokeInvalidLength = TxHelpers.invoke(dAppAcc.toAddress, Some("invalidLength"), invoker = dAppAcc)
    val invokeUnexisting    = TxHelpers.invoke(dAppAcc.toAddress, Some("unexisting"), invoker = dAppAcc)

    withDomain(RideV5) { d =>
      d.appendBlock(genesis, genesis2, setDApp, setDApp2)
      d.appendBlockE(invokeInvalidLength) should produce(s"Transfer error: invalid asset ID '$invalidLengthAsset' length = 4 bytes, must be 32")
      d.appendBlockE(invokeUnexisting) should produce(s"Transfer error: asset '$unexistingAsset' is not found on the blockchain")
    }
  }
}
