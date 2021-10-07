package com.wavesplatform.state.diffs.smart.eth

import com.esaulpaugh.headlong.abi.{Function, Tuple}
import com.esaulpaugh.headlong.util.FastHex
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
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

  property("invoke with payment") {
    val fee       = ciFee().sample.get
    val dApp      = accountGen.sample.get
    val issue     = IssueTransaction.selfSigned(2.toByte, dApp, "Asset", "", ENOUGH_AMT, 8, true, None, fee, ts).explicitGet()
    val asset     = IssuedAsset(issue.id())
    val script    = dAppScript(asset)
    val signature = ABIConverter(script).funcByMethodId.collectFirst { case (_, f) if f.name == "default" => f }.get
    val encodedCall = new Function(signature.ethSignature)
      .encodeCall(new Tuple(passingArg, Array[Tuple](new Tuple(asset.id.arr, paymentAmount))))
      .array()
    val hexData   = FastHex.encodeToString(encodedCall, 0, encodedCall.length)
    val invoke    = EthereumTransaction.Invocation(dApp.toAddress, hexData)
    val ethInvoke = EthereumTransaction(invoke, TestEthUnderlying, TestEthSignature, 'T'.toByte)
    val invoker   = ethInvoke.senderAddress()
    val transfer  = TransferTransaction.selfSigned(2.toByte, dApp, invoker, asset, ENOUGH_AMT, Waves, fee, ByteStr.empty, ts).explicitGet()
    val gTx1      = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
    val gTx2      = GenesisTransaction.create(dApp.toAddress, ENOUGH_AMT, ts).explicitGet()
    val setDApp   = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(script), fee, ts).explicitGet()

    withDomain(RideV6) { d =>
      d.appendBlock(gTx1, gTx2, setDApp, issue, transfer)
      d.appendBlock(ethInvoke)

      d.liquidDiff.portfolios(dApp.toAddress) shouldBe Portfolio.build(asset, paymentAmount)
      d.liquidDiff.portfolios(ethInvoke.senderAddress()) shouldBe Portfolio(-ethInvoke.underlying.getGasPrice.longValue(),
                                                                            assets = Map(asset -> -paymentAmount))
      d.liquidDiff.scriptsRun shouldBe 1
      d.liquidDiff.accountData(dApp.toAddress).data("check").value shouldBe true
    }
  }
}
