package com.wavesplatform.state.diffs.ci
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_LONG}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.FeeValidation.FeeUnit
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.TxVersion.{V1, V2}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class InvokeValidationTest extends PropSpec with WithDomain {
  import DomainPresets.*

  property("invoke by alias with wrong chainId") {
    withDomain(RideV5) { d =>
      val alias = Alias.fromString("alias:X:alias", Some('X'.toByte)).explicitGet()
      d.appendBlockE(invoke(alias)) should produce("Address belongs to another network: expected: 84(T), actual: 88(X)")
    }
  }

  property("invoke unexisting function") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val script = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func f() = []
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, script))
      d.appendBlockE(invoke(func = Some("g"))) should produce("Cannot find callable function `g`")
    }
  }

  property("invoke address without script") {
    withDomain(RideV5) { d =>
      d.appendBlockE(invoke()) should produce(s"No contract at address $secondAddress")
    }
  }

  property("invoke address with set expression") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val expression = TestCompiler(V5).compileExpression("true")
      d.appendBlock(setScript(secondSigner, expression))
      d.appendBlockE(invoke()) should produce("Trying to call dApp on the account with expression script")
    }
  }

  property("sync invoke address with set expression") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner, signer(2))) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = invoke(Address(base58'${signer(2).toAddress}'), "f", [], [])
           |   []
           | }
         """.stripMargin
      )
      val expression = TestCompiler(V5).compileExpression("true")
      d.appendBlock(setScript(secondSigner, dApp), setScript(signer(2), expression))
      d.appendBlockE(invoke()) should produce("Trying to call dApp on the account with expression script")
    }
  }

  property("invoke payment balance in Waves should be checked before script execution only if spending exceeds fee, in asset — always") {
    withDomain(RideV5, Seq(AddrWithBalance(secondAddress, ENOUGH_AMT), AddrWithBalance(signer(2).toAddress, invokeFee))) { d =>
      val script = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = if (true) then throw() else []
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, script))

      // Waves payment and fee
      d.appendBlockE(invoke(invoker = signer(2), payments = Seq(Payment(invokeFee, Waves)))) should produce(
        "Explicit script termination"
      )
      d.appendBlockE(invoke(invoker = signer(2), payments = Seq(Payment(invokeFee + 1, Waves)))) should produce(
        "Attempt to transfer unavailable funds: " +
          "Transaction application leads to negative waves balance to (at least) temporary negative state, " +
          "current balance equals 500000, spends equals -1000001, result is -500001"
      )

      // asset payment and Waves fee
      val i     = issue()
      val asset = IssuedAsset(i.id.value())
      d.appendBlock(i)
      d.appendBlockE(invoke(invoker = signer(2), payments = Seq(Payment(1, asset)))) should produce(
        "Attempt to transfer unavailable funds: " +
          s"Transaction application leads to negative asset '$asset' balance to (at least) temporary negative state, " +
          "current balance is 0, spends equals -1, result is -1"
      )

      // asset payment and asset fee
      d.appendBlock(sponsor(asset, Some(FeeUnit)), transfer(defaultSigner, signer(2).toAddress, invokeFee, asset))
      d.appendBlockE(invoke(invoker = signer(2), payments = Seq(Payment(1, asset)), feeAssetId = asset)) should produce(
        "Attempt to transfer unavailable funds: " +
          s"Transaction application leads to negative asset '$asset' balance to (at least) temporary negative state, " +
          "current balance is 500000, spends equals -500001, result is -1"
      )
    }
  }

  property("invoke tx size limit is 5 Kb") {
    def array(size: Int): ARR = ARR(Vector.fill(size)(CONST_LONG(1)), 0, false).explicitGet()

    val txV1       = invoke(defaultAddress, Some("f"), Seq(array(557)), version = V1)
    def tooBigTxV1 = invoke(defaultAddress, Some("f"), Seq(array(558)), version = V1)
    txV1.copy(proofs = Proofs.empty).bytes().length shouldBe 5114
    (the[Exception] thrownBy tooBigTxV1).getMessage shouldBe "GenericError(InvokeScriptTransaction bytes length = 5123 exceeds limit = 5120)"

    val txV2       = invoke(defaultAddress, Some("f"), Seq(array(564)), version = V2)
    def tooBigTxV2 = invoke(defaultAddress, Some("f"), Seq(array(565)), version = V2)
    PBTransactions.toPBInvokeScriptData(txV2.dApp, txV2.funcCallOpt, txV2.payments).toByteArray.length shouldBe 5120
    (the[Exception] thrownBy tooBigTxV2).getMessage shouldBe "GenericError(InvokeScriptTransaction bytes length = 5129 exceeds limit = 5120)"
  }

  property("unexisting payment asset") {
    withDomain(RideV5) { d =>
      val asset = IssuedAsset(ByteStr.fromBytes(1, 2, 3))
      d.appendBlockE(invoke(defaultAddress, payments = Seq(Payment(1, asset)))) should produce(
        "Attempt to transfer unavailable funds: " +
          s"Transaction application leads to negative asset '$asset' balance to (at least) temporary negative state, " +
          "current balance is 0, spends equals -1, result is -1"
      )
    }
  }
}
