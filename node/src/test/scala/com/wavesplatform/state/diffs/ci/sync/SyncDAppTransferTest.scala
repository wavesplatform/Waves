package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, FUNC, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.FunctionIds.CREATE_LIST
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, TxHelpers}
import org.scalatest.Inside

class SyncDAppTransferTest extends PropSpec with WithDomain with Inside {

  property("negative transfer amount") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

      val issue      = TxHelpers.issue(dApp2, 100)
      val asset      = IssuedAsset(issue.id.value())
      val setScript1 = TxHelpers.setScript(dApp1, invokerDAppScript(dApp2.toAddress, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, simpleTransferDAppScript(amount = -1, asset = asset, bigComplexity = bigComplexityDApp2))

      val preparingTxs = Seq(issue, setScript1, setScript2)

      val invoke = TxHelpers.invoke(dApp1.toAddress, func = Some("foo"), invoker = invoker)

      withDomain(
        RideV5
          .configure(_.copy(enforceTransferValidationAfter = 3))
          .setFeaturesHeight(BlockchainFeatures.RideV6 -> 4),
        balances
      ) { d =>
        d.appendBlock(preparingTxs*)

        d.appendAndCatchError(invoke).toString should include("Negative transfer amount")

        d.appendBlock()

        if (!bigComplexityDApp1 && !bigComplexityDApp2) {
          d.appendAndCatchError(invoke).toString should include("Negative transfer amount")
        } else {
          d.appendAndAssertFailed(invoke)
        }
      }
    }
  }

  property("negative balance rejects or fails tx") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp1) :+ AddrWithBalance(dApp2.toAddress, 0.01.waves)

      val setScript1 = TxHelpers.setScript(dApp1, invokerWithTransferDAppScript(dApp2.toAddress, 100, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, simpleTransferDAppScript(amount = 100, bigComplexity = bigComplexityDApp2))

      val preparingTxs = Seq(setScript1, setScript2)

      val invoke = TxHelpers.invoke(dApp1.toAddress, func = Some("foo"), invoker = invoker)

      withDomain(
        RideV5
          .configure(_.copy(enforceTransferValidationAfter = 3))
          .setFeaturesHeight(BlockchainFeatures.RideV6 -> 4),
        balances
      ) { d =>
        d.appendBlock(preparingTxs*)

        d.appendAndCatchError(invoke).toString should include("Negative waves balance")

        d.appendBlock()

        if (!bigComplexityDApp1 && !bigComplexityDApp2) {
          d.appendAndCatchError(invoke).toString should include("negative waves balance")
        } else {
          d.appendAndAssertFailed(invoke)
        }
      }
    }
  }

  property("invoking ScriptTransfer in sync call results in accounts state") {
    val invoker     = TxHelpers.signer(0)
    val invokerDApp = TxHelpers.signer(1)
    val senderDApp  = TxHelpers.signer(2)
    val recipient   = TxHelpers.signer(3)

    val transferAmount = 10.waves

    val genesis          = Seq(invoker, invokerDApp, senderDApp).map(acc => TxHelpers.genesis(acc.toAddress))
    val setInvokerScript = TxHelpers.setScript(invokerDApp, invokerDAppScript(senderDApp.toAddress))
    val setSenderScript  = TxHelpers.setScript(senderDApp, simpleTransferDAppScript(amount = transferAmount, recipient = Some(recipient.toAddress)))
    val invoke           = TxHelpers.invoke(invokerDApp.toAddress, func = Some("foo"), invoker = invoker)

    assertDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq(setInvokerScript, setSenderScript))),
      TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
      RideV5.blockchainSettings.functionalitySettings
    ) { case (blockDiff, _) =>
      inside(blockDiff.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          call2.stateChanges.error shouldBe empty
          call2.stateChanges.invokes shouldBe empty
        }
      }
      blockDiff.portfolios(recipient.toAddress).balance shouldBe transferAmount
      blockDiff.portfolios(senderDApp.toAddress).balance shouldBe -transferAmount
      blockDiff.transaction(invoke.id()) shouldBe defined
    }
  }

  property("invoking default func ScriptTransfer in sync call results in accounts state") {
    val invoker     = TxHelpers.signer(0)
    val invokerDApp = TxHelpers.signer(1)
    val senderDApp  = TxHelpers.signer(2)
    val recipient   = TxHelpers.signer(3)

    val transferAmount = 10.waves

    val genesis          = Seq(invoker, invokerDApp, senderDApp).map(acc => TxHelpers.genesis(acc.toAddress))
    val setInvokerScript = TxHelpers.setScript(invokerDApp, invokerDAppScript(senderDApp.toAddress))
    val setSenderScript  = TxHelpers.setScript(senderDApp, simpleTransferDAppScript(amount = transferAmount, recipient = Some(recipient.toAddress)))
    val invoke           = TxHelpers.invoke(invokerDApp.toAddress, invoker = invoker)

    assertDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq(setInvokerScript, setSenderScript))),
      TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
      RideV5.blockchainSettings.functionalitySettings
    ) { case (blockDiff, _) =>
      inside(blockDiff.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          call2.stateChanges.error shouldBe empty
          call2.stateChanges.invokes shouldBe empty
        }
      }
      blockDiff.portfolios(recipient.toAddress).balance shouldBe transferAmount
      blockDiff.portfolios(senderDApp.toAddress).balance shouldBe -transferAmount
      blockDiff.transaction(invoke.id()) shouldBe defined
    }
  }

  property("self-transfer in sync call is forbidden") {
    val invoker     = TxHelpers.signer(0)
    val invokerDApp = TxHelpers.signer(1)
    val senderDApp  = TxHelpers.signer(2)

    withDomain(RideV5, AddrWithBalance.enoughBalances(invoker, invokerDApp, senderDApp)) { d =>
      val setInvokerScript = TxHelpers.setScript(invokerDApp, invokerDAppScript(senderDApp.toAddress))
      val setSenderScript  = TxHelpers.setScript(senderDApp, simpleTransferDAppScript(recipient = Some(senderDApp.toAddress)))
      val invoke           = TxHelpers.invoke(invokerDApp.toAddress, invoker = invoker)

      d.appendBlock(setInvokerScript, setSenderScript)

      d.appendBlockE(invoke) should produce("DApp self-transfer is forbidden since V4")
    }
  }

  property("ScriptTransfer in sync call is allowed if funds were received from attached payment") {
    val invoker     = TxHelpers.signer(0)
    val invokerDApp = TxHelpers.signer(1)
    val senderDApp  = TxHelpers.signer(2)
    val recipient   = TxHelpers.signer(3)

    val transferAmount = 10.waves

    val setInvokerScript = TxHelpers.setScript(invokerDApp, invokerDAppScript(senderDApp.toAddress, paymentAmount = Some(transferAmount)))
    val setSenderScript  = TxHelpers.setScript(senderDApp, simpleTransferDAppScript(amount = transferAmount, recipient = Some(recipient.toAddress)))
    val genesis = Seq(invoker, invokerDApp).map(acc => TxHelpers.genesis(acc.toAddress)) :+ TxHelpers.genesis(
      senderDApp.toAddress,
      setSenderScript.fee.value
    )
    val invoke = TxHelpers.invoke(invokerDApp.toAddress, func = Some("foo"), invoker = invoker)

    assertDiffAndState(
      Seq(TestBlock.create(genesis ++ Seq(setInvokerScript, setSenderScript))),
      TestBlock.create(Seq(invoke), Block.ProtoBlockVersion),
      RideV5.blockchainSettings.functionalitySettings
    ) { case (blockDiff, _) =>
      inside(blockDiff.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          call2.stateChanges.error shouldBe empty
          call2.stateChanges.invokes shouldBe empty
        }
      }
      blockDiff.portfolios(recipient.toAddress).balance shouldBe transferAmount
      blockDiff.portfolios(invokerDApp.toAddress).balance shouldBe -transferAmount
      blockDiff.transaction(invoke.id()) shouldBe defined
    }
  }

  property("ScriptTransfer with empty amount field in sync call should be failed") {
    val invoker     = TxHelpers.signer(0)
    val invokerDApp = TxHelpers.signer(1)
    val senderDApp  = TxHelpers.signer(2)

    withDomain(RideV5, AddrWithBalance.enoughBalances(invoker, invokerDApp, senderDApp)) { d =>
      val setInvokerScript = TxHelpers.setScript(invokerDApp, invokerDAppScript(senderDApp.toAddress))
      val setSenderScript  = TxHelpers.setScript(senderDApp, transferWithEmptyAmountDAppScript)
      val invoke           = TxHelpers.invoke(invokerDApp.toAddress, func = Some("foo"), invoker = invoker)

      d.appendBlock(setInvokerScript, setSenderScript)

      d.createDiffE(invoke) should produceRejectOrFailedDiff("key not found: asset")
    }
  }

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def invokerDAppScript(dApp2: Address, bigComplexity: Boolean = false, paymentAmount: Option[Long] = None): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke(unit, [], [${paymentAmount.map(amount => s"AttachedPayment(unit, $amount)").getOrElse("")}])
         |    []
         | }
         | 
         | @Callable(i)
         | func foo() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke("bar", [], [${paymentAmount.map(amount => s"AttachedPayment(unit, $amount)").getOrElse("")}])
         |    []
         | }
       """.stripMargin
    )

  private def invokerWithTransferDAppScript(dApp2: Address, amount: Long, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func foo() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke("bar", [], [])
         |    [
         |      ScriptTransfer(Address(base58'$dApp2'), $amount, unit)
         |    ]
         | }
       """.stripMargin
    )

  private def simpleTransferDAppScript(
      amount: Long = 1.waves,
      asset: Asset = Waves,
      recipient: Option[Address] = None,
      bigComplexity: Boolean = false
  ): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func bar() = {
         |   ${sigVerify(bigComplexity)}
         |   let asset = ${asset.fold(GlobalValNames.Unit)(a => s"base58'$a'")}
         |   [
         |     ScriptTransfer(${recipient.map(addr => s"Address(base58'$addr')").getOrElse("i.caller")}, $amount, asset)
         |   ]
         | }
         | 
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   let asset = ${asset.fold(GlobalValNames.Unit)(a => s"base58'$a'")}
         |   [
         |     ScriptTransfer(${recipient.map(addr => s"Address(base58'$addr')").getOrElse("i.caller")}, $amount, asset)
         |   ]
         | }
       """.stripMargin
    )

  private def transferWithEmptyAmountDAppScript: Script = {
    val dAppResult = FUNCTION_CALL(
      Native(CREATE_LIST),
      List(
        FUNCTION_CALL(
          User("ScriptTransfer"),
          List(FUNCTION_CALL(User("Alias"), List(CONST_STRING("alias").explicitGet())), REF(GlobalValNames.Unit))
        ),
        REF(GlobalValNames.Nil)
      )
    )

    ContractScriptImpl(V5, DApp(DAppMeta(), Nil, List(CallableFunction(CallableAnnotation("i"), FUNC("bar", Nil, dAppResult))), None))
  }

}
