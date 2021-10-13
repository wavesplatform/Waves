package com.wavesplatform.state.diffs.smart.eth

import com.esaulpaugh.headlong.abi.{Function, Tuple}
import com.esaulpaugh.headlong.util.FastHex
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.diffs.smart.predef.{assertProvenPart, provenPart}
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{ABIConverter, Asset, EthereumTransaction, GenesisTransaction}
import com.wavesplatform.utils.EthHelpers

class EthereumInvokeTest extends PropSpec with WithDomain with EthHelpers {
  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val passingArg    = 123L
  private val paymentAmount = 456L

  private def assetScript(tx: EthereumTransaction, dApp: Address) = TestCompiler(V6).compileAsset {
    s"""
       | match tx {
       |   case t: InvokeScriptTransaction => ${checkEthInvoke(tx, dApp)}
       |   case _                          => true
       | }
     """.stripMargin
  }

  private def checkEthInvoke(tx: EthereumTransaction, dApp: Address): String =
    s"""
       | ${provenPart(tx, emptyBodyBytes = true, checkProofs = false)}
       | let dAppAddress = match t.dApp {
       |   case a: Address => a.bytes == base58'$dApp'
       |   case _: Alias   => throw()
       | }
       | let feeAssetId = t.feeAssetId == unit
       | let checkFunc  = t.function == "default"
       | let checkArgs  = t.args == [$passingArg]
       | let payments   = t.payments == [ AttachedPayment(this.id, $paymentAmount) ]
       | ${assertProvenPart("t", proofs = false)} && dAppAddress && feeAssetId && checkFunc && checkArgs && payments
     """.stripMargin

  private def dAppScript(asset: Asset) = TestCompiler(V6).compileContract(
    s"""
       | @Callable(i)
       | func default(value: Int) = {
       |   let check =
       |     value == $passingArg &&
       |     i.payments == [ AttachedPayment(base58'$asset', $paymentAmount) ]
       |   [ BooleanEntry("check", check) ]
       | }
     """.stripMargin
  )

  private def hexData(script: Script, asset: IssuedAsset) = {
    val signature = ABIConverter(script).funcByMethodId.collectFirst { case (_, f) if f.name == "default" => f }.get
    val args      = new Tuple(passingArg, Array[Tuple](new Tuple(asset.id.arr, paymentAmount)))
    val call      = new Function(signature.ethSignature).encodeCall(args).array()
    FastHex.encodeToString(call, 0, call.length)
  }

  property("invoke with scripted payment") {
    val fee            = ciFee().sample.get
    val dApp           = accountGen.sample.get
    val dummyInvoke    = EthereumTransaction.Invocation(dApp.toAddress, "")
    val dummyEthInvoke = EthereumTransaction(dummyInvoke, TestEthUnderlying, TestEthSignature, 'T'.toByte)  // needed to pass into asset script
    val aScript        = assetScript(dummyEthInvoke, dApp.toAddress)
    val issue          = IssueTransaction.selfSigned(2.toByte, dApp, "Asset", "", ENOUGH_AMT, 8, true, Some(aScript), fee, ts).explicitGet()
    val asset          = IssuedAsset(issue.id())
    val script         = dAppScript(asset)
    val invoke         = EthereumTransaction.Invocation(dApp.toAddress, hexData(script, asset))
    val ethInvoke      = dummyEthInvoke.copy(invoke)
    val invoker        = ethInvoke.senderAddress()
    val transfer       = TransferTransaction.selfSigned(2.toByte, dApp, invoker, asset, ENOUGH_AMT, Waves, fee, ByteStr.empty, ts).explicitGet()
    val gTx1           = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
    val gTx2           = GenesisTransaction.create(dApp.toAddress, ENOUGH_AMT, ts).explicitGet()
    val setDApp        = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(script), fee, ts).explicitGet()

    withDomain(RideV6) { d =>
      d.appendBlock(gTx1, gTx2, setDApp, issue, transfer)
      d.appendBlock(ethInvoke)

      d.liquidDiff.errorMessage(ethInvoke.id()) shouldBe None
      d.liquidDiff.portfolios(dApp.toAddress) shouldBe Portfolio.build(asset, paymentAmount)
      d.liquidDiff.portfolios(ethInvoke.senderAddress()) shouldBe Portfolio(-ethInvoke.underlying.getGasPrice.longValue(),
                                                                            assets = Map(asset -> -paymentAmount))
      d.liquidDiff.scriptsRun shouldBe 2
      d.liquidDiff.accountData(dApp.toAddress).data("check").value shouldBe true
    }
  }
}
