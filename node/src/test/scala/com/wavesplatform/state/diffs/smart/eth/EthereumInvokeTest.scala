package com.wavesplatform.state.diffs.smart.eth

import cats.implicits._
import com.esaulpaugh.headlong.abi.{Function, Tuple}
import com.esaulpaugh.headlong.util.FastHex
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments._
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.diffs.smart.predef.{assertProvenPart, provenPart}
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SetAssetScriptTransaction}
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

  private def assetScript(tx: EthereumTransaction, dApp: Address, assets: Seq[Asset], version: StdLibVersion): Script =
    TestCompiler(version).compileAsset {
      s"""
       | match tx {
       |   case t: InvokeScriptTransaction => ${checkEthInvoke(tx, dApp, assets, version)}
       |   case _                          => true
       | }
     """.stripMargin
    }

  private def checkEthInvoke(tx: EthereumTransaction, dApp: Address, assets: Seq[Asset], version: StdLibVersion): String = {
    val payments = assets.map(a => s""" AttachedPayment(base58'$a', $paymentAmount) """).mkString(", ")
    s"""
       | ${provenPart(tx, emptyBodyBytes = true, checkProofs = false)}
       | let dAppAddress = match t.dApp {
       |   case a: Address => a.bytes == base58'$dApp'
       |   case _: Alias   => throw()
       | }
       | let feeAssetId = t.feeAssetId == unit
       | let checkFunc  = t.function == "default"
       | let checkArgs  = t.args == [$passingArg]
       | let payments   = ${if (version > V3) s"t.payments == [$payments]" else s"t.payment == ${if (payments.nonEmpty) payments else "unit"}"}
       | ${assertProvenPart("t", proofs = false)} && dAppAddress && feeAssetId && checkFunc && checkArgs && payments
     """.stripMargin
  }

  private def dAppScript(assets: Seq[Asset], version: StdLibVersion) = TestCompiler(version).compileContract {
    val payments = assets.map(a => s""" AttachedPayment(base58'$a', $paymentAmount) """).mkString(", ")
    s"""
       | @Callable(i)
       | func default(value: Int) = {
       |   let check =
       |     value == $passingArg &&
       |     ${if (version > V3) s"i.payments == [$payments]" else s"i.payment == ${if (payments.nonEmpty) payments else "unit"}"}
       |
       |   ${if (version > V3) """[ BooleanEntry("check", check) ]""" else """ WriteSet([DataEntry("check", check)]) """}
       | }
     """.stripMargin
  }

  private def hexData(script: Script, assets: Seq[IssuedAsset]) = {
    val signature = ABIConverter(script).funcByMethodId.collectFirst { case (_, f) if f.name == "default" => f }.get
    val args      = new Tuple(passingArg, Array[Tuple](assets.map(a => new Tuple(a.id.arr, paymentAmount)): _*))
    val call      = new Function(signature.ethSignature).encodeCall(args).array()
    FastHex.encodeToString(call, 0, call.length)
  }

  private def preconditions(dAppVersion: StdLibVersion, assetScriptVersion: StdLibVersion, paymentCount: Int) = {
    val fee  = ciFee().sample.get
    val dApp = accountGen.sample.get

    val dummyInvoke    = EthereumTransaction.Invocation(dApp.toAddress, "")
    val dummyEthInvoke = EthereumTransaction(dummyInvoke, TestEthUnderlying, TestEthSignature, 'T'.toByte) // needed to pass into asset script
    val invoker        = dummyEthInvoke.senderAddress()

    val emptyScript = Some(ExprScript(Terms.TRUE).explicitGet())
    val issues =
      (1 to paymentCount).map(_ => IssueTransaction.selfSigned(2.toByte, dApp, "Asset", "", ENOUGH_AMT, 8, true, emptyScript, fee, ts).explicitGet())
    val assets          = issues.map(i => IssuedAsset(i.id()))
    val resultScript    = assetScript(dummyEthInvoke, dApp.toAddress, assets, assetScriptVersion)
    val setAssetScripts = assets.map(a => SetAssetScriptTransaction.selfSigned(1.toByte, dApp, a, Some(resultScript), fee, ts).explicitGet())
    val assetTransfers =
      assets.map(a => TransferTransaction.selfSigned(2.toByte, dApp, invoker, a, ENOUGH_AMT, Waves, fee, ByteStr.empty, ts).explicitGet())

    val script  = dAppScript(assets, dAppVersion)
    val setDApp = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(script), fee, ts).explicitGet()

    val invoke    = EthereumTransaction.Invocation(dApp.toAddress, hexData(script, assets))
    val ethInvoke = dummyEthInvoke.copy(invoke)

    val gTx1 = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
    val gTx2 = GenesisTransaction.create(dApp.toAddress, ENOUGH_AMT, ts).explicitGet()

    (List(gTx1, gTx2, setDApp) ++ issues ++ assetTransfers ++ setAssetScripts, ethInvoke, dApp.toAddress, assets)
  }

  private def assert(dAppVersion: StdLibVersion, assetScriptVersion: StdLibVersion, paymentCount: Int) = {
    val (preparingTxs, ethInvoke, dApp, assets) = preconditions(dAppVersion, assetScriptVersion, paymentCount)
    withDomain(RideV6) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(ethInvoke)

      d.liquidDiff.errorMessage(ethInvoke.id()) shouldBe None
      d.liquidDiff.accountData(dApp).data("check").value shouldBe true

      val assetsPortfolio = assets.map(Portfolio.build(_, paymentAmount)).fold(Portfolio())(_ |+| _)
      d.liquidDiff.portfolios.getOrElse(dApp, Portfolio()) shouldBe assetsPortfolio
      d.liquidDiff.portfolios(ethInvoke.senderAddress()) shouldBe Portfolio(-ethInvoke.underlying.getGasPrice.longValue()).minus(assetsPortfolio)
      d.liquidDiff.scriptsRun shouldBe 1 + paymentCount
    }
  }

  property("invoke with scripted payments") {
    val allVersions  = DirectiveDictionary[StdLibVersion].all
    val lastVersion  = allVersions.last
    val dAppVersions = allVersions.filter(_ >= V3)

    dAppVersions.foreach { v =>
      assert(dAppVersion = v, assetScriptVersion = lastVersion, v.maxPayments)
      assert(dAppVersion = v, assetScriptVersion = lastVersion, 0)
      assert(dAppVersion = lastVersion, assetScriptVersion = v, v.maxPayments)
      assert(dAppVersion = lastVersion, assetScriptVersion = v, 0)
    }
  }
}
