package com.wavesplatform.state.diffs.smart.eth

import com.esaulpaugh.headlong.abi.{Function, Tuple}
import com.esaulpaugh.headlong.util.FastHex
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments.*
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.diffs.smart.predef.{assertProvenPart, provenPart}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SetAssetScriptTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{ABIConverter, Asset, EthereumTransaction, GenesisTransaction}
import com.wavesplatform.utils.EthHelpers

class EthereumInvokeTest extends PropSpec with WithDomain with EthHelpers {
  import DomainPresets.*

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val passingArg    = 123L
  private val paymentAmount = 456L

  private def assetScript(tx: EthereumTransaction, dApp: Address, assets: Seq[Asset], currentAsset: Asset, version: StdLibVersion): Script =
    TestCompiler(version).compileAsset {
      s"""
         | match tx {
         |   case t: InvokeScriptTransaction => ${checkEthInvoke(tx, dApp, assets, currentAsset, version)}
         |   case _                          => true
         | }
     """.stripMargin
    }

  private def checkEthInvoke(tx: EthereumTransaction, dApp: Address, assets: Seq[Asset], currentAsset: Asset, version: StdLibVersion): String = {
    val payments = assets.map(a => s""" AttachedPayment(base58'$a', $paymentAmount) """).mkString(", ")
    s"""
       | ${provenPart(tx, emptyBodyBytes = true, checkProofs = false)}
       | let dAppAddress = match t.dApp {
       |   case a: Address => a.bytes == base58'$dApp'
       |   case _: Alias   => throw()
       | }
       | let checkId    = this.id == base58'$currentAsset'
       | let feeAssetId = t.feeAssetId == unit
       | let checkFunc  = t.function == "default"
       | let checkArgs  = t.args == [$passingArg]
       | let payments   = ${if (version > V3) s"t.payments == [$payments]" else s"t.payment == ${if (payments.nonEmpty) payments else "unit"}"}
       | ${assertProvenPart("t", proofs = false)} && dAppAddress && feeAssetId && checkFunc && checkArgs && payments && checkId
     """.stripMargin
  }

  private def makeDAppScript(assets: Seq[Asset], dAppAddress: Address, version: StdLibVersion, syncCall: Boolean) =
    TestCompiler(version).compileContract {
      val payments = assets.map(a => s""" AttachedPayment(base58'$a', $paymentAmount) """).mkString(", ")
      s"""
         | @Callable(i)
         | func default(value: Int) = {
         |   ${if (syncCall) s""" strict r = invoke(Address(base58'$dAppAddress'), "default", [], []) """ else ""}
         |   let check =
         |     value == $passingArg &&
         |     ${if (version > V3) s"i.payments == [$payments]" else s"i.payment == ${if (payments.nonEmpty) payments else "unit"}"}
         |
         |   ${if (version > V3) """[ BooleanEntry("check", check) ]""" else """ WriteSet([DataEntry("check", check)]) """}
         | }
     """.stripMargin
    }

  private def makeDAppScript2(version: StdLibVersion, thisDApp: KeyPair, callingDApp: KeyPair, invoker: Address, invokerPk: PublicKey): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   let check =
         |     this                    == Address(base58'${thisDApp.toAddress}')    &&
         |     i.caller                == Address(base58'${callingDApp.toAddress}') &&
         |     i.originCaller          == Address(base58'$invoker')                 &&
         |     i.callerPublicKey       == base58'${callingDApp.publicKey}'          &&
         |     i.originCallerPublicKey == base58'$invokerPk'                        &&
         |     i.fee                   == ${EthereumTransaction.GasPrice}           &&
         |     i.payments              == []                                        &&
         |     i.feeAssetId            == unit
         |   ${if (version > V3) """[ BooleanEntry("check", check) ]""" else """ WriteSet([DataEntry("check", check)]) """}
         | }
       """.stripMargin
    )

  private def hexData(script: Script, assets: Seq[IssuedAsset]) = {
    val signature = ABIConverter(script).funcByMethodId.collectFirst { case (_, f) if f.name == "default" => f }.get
    val args      = new Tuple(passingArg, Array[Tuple](assets.map(a => new Tuple(a.id.arr, paymentAmount))*))
    val call      = new Function(signature.ethSignature).encodeCall(args).array()
    FastHex.encodeToString(call, 0, call.length)
  }

  private def preconditions(dAppVersion: StdLibVersion, assetScriptVersion: StdLibVersion, paymentCount: Int, syncCall: Boolean) = {
    val fee   = ciFee().sample.get
    val dApp  = RandomKeyPair()
    val dApp2 = RandomKeyPair()

    val dummyInvoke    = EthereumTransaction.Invocation(dApp.toAddress, "")
    val dummyEthInvoke = EthereumTransaction(dummyInvoke, TestEthRawTransaction, TestEthSignature, 'T'.toByte) // needed to pass into asset script
    val invoker        = dummyEthInvoke.senderAddress()
    val invokerPk      = dummyEthInvoke.signerPublicKey()

    val emptyScript = Some(ExprScript(Terms.TRUE).explicitGet())
    val issues =
      (1 to paymentCount).map(_ =>
        IssueTransaction.selfSigned(2.toByte, dApp, "Asset", "", ENOUGH_AMT, 8, true, emptyScript, 1.waves, ts).explicitGet()
      )
    val assets = issues.map(i => IssuedAsset(i.id()))
    val setAssetScripts = assets.map { asset =>
      val resultScript = assetScript(dummyEthInvoke, dApp.toAddress, assets, asset, assetScriptVersion)
      SetAssetScriptTransaction.selfSigned(1.toByte, dApp, asset, Some(resultScript), 1.waves, ts).explicitGet()
    }
    val assetTransfers =
      assets.map(a => TransferTransaction.selfSigned(2.toByte, dApp, invoker, a, ENOUGH_AMT, Waves, fee, ByteStr.empty, ts).explicitGet())

    val dAppScript  = makeDAppScript(assets, dApp2.toAddress, dAppVersion, syncCall)
    val dAppScript2 = makeDAppScript2(if (dAppVersion >= V5) dAppVersion else V5, dApp2, dApp, invoker, invokerPk)
    val setDApp     = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(dAppScript), fee, ts).explicitGet()
    val setDApp2    = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dAppScript2), fee, ts).explicitGet()

    val invoke    = EthereumTransaction.Invocation(dApp.toAddress, hexData(dAppScript, assets))
    val ethInvoke = dummyEthInvoke.copy(invoke)

    val gTxs = Seq(invoker, dApp.toAddress, dApp2.toAddress).map(a => GenesisTransaction.create(a, ENOUGH_AMT, ts).explicitGet())

    (gTxs ++ List(setDApp, setDApp2) ++ issues ++ assetTransfers ++ setAssetScripts, ethInvoke, dApp.toAddress, dApp2.toAddress, assets)
  }

  private def assert(dAppVersion: StdLibVersion, assetScriptVersion: StdLibVersion, paymentCount: Int, syncCall: Boolean = false) = {
    val (preparingTxs, ethInvoke, dApp, dApp2, assets) = preconditions(dAppVersion, assetScriptVersion, paymentCount, syncCall)
    withDomain(RideV6) { d =>
      d.appendBlock(preparingTxs*)
      d.appendBlock(ethInvoke)

      d.liquidDiff.errorMessage(ethInvoke.id()) shouldBe None
      d.liquidDiff.accountData(dApp).data("check").value shouldBe true
      if (syncCall) d.liquidDiff.accountData(dApp2).data("check").value shouldBe true

      val assetsPortfolio = assets.map(Portfolio.build(_, paymentAmount)).fold(Portfolio())((p1, p2) => p1.combine(p2).explicitGet())
      d.liquidDiff.portfolios.getOrElse(dApp, Portfolio()) shouldBe assetsPortfolio
      d.liquidDiff.portfolios(ethInvoke.senderAddress()) shouldBe Portfolio(-ethInvoke.underlying.getGasPrice.longValue()).minus(assetsPortfolio)
      d.liquidDiff.scriptsRun shouldBe paymentCount + 1 + (if (syncCall) 1 else 0)
    }
  }

  property("invoke with scripted payments") {
    val allVersions  = DirectiveDictionary[StdLibVersion].all
    val lastVersion  = allVersions.last
    val dAppVersions = allVersions.filter(_ >= V3)

    dAppVersions.foreach { v =>
      assert(dAppVersion = v, assetScriptVersion = lastVersion, v.maxPayments, syncCall = v >= V5)
      assert(dAppVersion = v, assetScriptVersion = lastVersion, 0)
      assert(dAppVersion = lastVersion, assetScriptVersion = v, v.maxPayments)
      assert(dAppVersion = lastVersion, assetScriptVersion = v, 0)
    }
  }
}
