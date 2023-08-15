package com.wavesplatform.state.diffs.ci

import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.{V4, V5}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.{Diff, InvokeScriptResult, NewTransactionInfo, Portfolio}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.{invoke, secondSigner, setScript}
import org.scalatest.{EitherValues, Inside}

import scala.collection.immutable.VectorMap

class InvokeAssetChecksTest extends PropSpec with Inside with WithState with DBCacheSettings with WithDomain with EitherValues {
  import DomainPresets.*

  private val invalidLengthAsset = IssuedAsset(ByteStr.decodeBase58("WAVES").get)
  private val nonExistentAsset   = IssuedAsset(ByteStr.decodeBase58("WAVESwavesWAVESwavesWAVESwavesWAVESwaves123").get)

  private val lengthError      = s"Transfer error: invalid asset ID '$invalidLengthAsset' length = 4 bytes, must be 32"
  private val nonExistentError = s"Transfer error: asset '$nonExistentAsset' is not found on the blockchain"

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
         |    ScriptTransfer(i.caller, 0, base58'$nonExistentAsset')
         |  ]
       """.stripMargin
    )

    for {
      activated <- Seq(true, false)
      func      <- Seq("invalidLength", "unexisting")
    } {
      {
        val miner       = TxHelpers.signer(0).toAddress
        val invoker     = TxHelpers.signer(1)
        val master      = TxHelpers.signer(2)
        val balances    = AddrWithBalance.enoughBalances(invoker, master)
        val setScriptTx = TxHelpers.setScript(master, dApp)
        val invoke      = TxHelpers.invoke(master.toAddress, Some(func), invoker = invoker)

        val dAppAddress = master.toAddress

        def invokeInfo(succeeded: Boolean): Vector[NewTransactionInfo] = {
          val status = if (succeeded) Status.Succeeded else Status.Failed
          Vector(NewTransactionInfo(invoke, Set(invoke.senderAddress, dAppAddress), status, if (!succeeded) 8L else 18L))
        }

        val expectedResult =
          if (activated) {
            val expectingMessage =
              if (func == "invalidLength")
                lengthError
              else
                nonExistentError
            Diff.withTransactions(
              invokeInfo(false),
              portfolios = Map(
                invoke.senderAddress -> Portfolio(-invoke.fee.value),
                miner                -> Portfolio((setScriptTx.fee.value * 0.6 + invoke.fee.value * 0.4).toLong + 6.waves)
              ),
              scriptsComplexity = 8,
              scriptResults = Map(invoke.id() -> InvokeScriptResult(error = Some(ErrorMessage(1, expectingMessage))))
            )
          } else {
            val asset = if (func == "invalidLength") invalidLengthAsset else nonExistentAsset
            Diff.withTransactions(
              invokeInfo(true),
              portfolios = Map(
                invoke.senderAddress -> Portfolio(-invoke.fee.value, assets = VectorMap(asset -> 0)),
                dAppAddress          -> Portfolio.build(asset, 0),
                miner                -> Portfolio((setScriptTx.fee.value * 0.6 + invoke.fee.value * 0.4).toLong + 6.waves)
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

  property("attached invoke payment asset checks") {
    val sigVerify = s"""strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}"""
    def dApp(complex: Boolean) = TestCompiler(V5).compileContract(
      s"""
         |@Callable(i)
         |func invalidLength() = {
         |  ${if (complex) sigVerify else ""}
         |  strict r = invoke(this, "default", [], [AttachedPayment(base58'$invalidLengthAsset', 1)])
         |  []
         |}
         |
         |@Callable(i)
         |func unexisting() = {
         |  ${if (complex) sigVerify else ""}
         |  strict r = invoke(this, "default", [], [AttachedPayment(base58'$nonExistentAsset', 1)])
         |  []
         |}
         |
         |@Callable(i)
         |func default() = []
       """.stripMargin
    )
    Seq(true, false).foreach { complex =>
      withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
        d.appendBlock(setScript(secondSigner, dApp(complex)))
        val invalidLengthInvoke   = invoke(func = Some("invalidLength"))
        val unexistingErrorInvoke = invoke(func = Some("unexisting"))
        if (complex) {
          d.appendAndAssertFailed(invalidLengthInvoke, lengthError)
          d.appendAndAssertFailed(unexistingErrorInvoke, nonExistentError)
        } else {
          d.appendBlockE(invalidLengthInvoke) should produce(lengthError)
          d.appendBlockE(unexistingErrorInvoke) should produce(nonExistentError)
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
         |  strict r = invoke(callingDApp, "default", [], [AttachedPayment(base58'$nonExistentAsset', 1)])
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
      d.appendBlockE(invokeInvalidLength) should produce(lengthError)
      d.appendBlockE(invokeUnexisting) should produce(nonExistentError)
    }
  }

  property("issuing asset name and description limits") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      Seq(false, true).foreach { complex =>
        val sigVerify = s"""strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")} """
        def dApp(name: String = "name", description: String = "") = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() = [
             |   ${if (complex) sigVerify else ""}
             |   Issue("$name", "$description", 1000, 4, true, unit, 0)
             | ]
         """.stripMargin
        )

        def invokeTx = invoke(fee = invokeFee(issues = 1))

        d.appendBlock(setScript(secondSigner, dApp("a" * 3)))
        d.appendAndAssertFailed(invokeTx, "Invalid asset name")
        d.appendBlock(setScript(secondSigner, dApp("a" * 4)))
        d.appendAndAssertSucceed(invokeTx)

        d.appendBlock(setScript(secondSigner, dApp("a" * 17)))
        d.appendAndAssertFailed(invokeTx, "Invalid asset name")
        d.appendBlock(setScript(secondSigner, dApp("a" * 16)))
        d.appendAndAssertSucceed(invokeTx)

        d.appendBlock(setScript(secondSigner, dApp(description = "a" * 1001)))
        d.appendAndAssertFailed(invokeTx, "Invalid asset description")
        d.appendBlock(setScript(secondSigner, dApp(description = "a" * 1000)))
        d.appendAndAssertSucceed(invokeTx)
      }
    }
  }

  property("issuing asset decimals limits") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      def dApp(decimals: Int) = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   Issue("name", "", 1000, $decimals, true, unit, 0)
           | ]
         """.stripMargin
      )

      def invokeTx = invoke(fee = invokeFee(issues = 1))

      d.appendBlock(setScript(secondSigner, dApp(-1)))
      d.appendBlockE(invokeTx) should produce("Invalid decimals -1")
      d.appendBlock(setScript(secondSigner, dApp(0)))
      d.appendAndAssertSucceed(invokeTx)

      d.appendBlock(setScript(secondSigner, dApp(9)))
      d.appendBlockE(invokeTx) should produce("Invalid decimals 9")
      d.appendBlock(setScript(secondSigner, dApp(8)))
      d.appendAndAssertSucceed(invokeTx)
    }
  }

  property("Issues with same nonces are allowed when any field differs") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   Issue("name", "", 1000, 1, true, unit, 0),
           |   Issue("name", "", 1000, 0, true, unit, 0)
           | ]
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertSucceed(invoke(fee = invokeFee(issues = 2)))
      d.liquidDiff.issuedAssets.size shouldBe 2
    }
  }
}
