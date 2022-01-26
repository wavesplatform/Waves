package com.wavesplatform.state.diffs.smart.eth

import cats.implicits.*
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments.*
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.{ciFee, toEthInvokeExpression}
import com.wavesplatform.state.diffs.smart.predef.{assertProvenPart, provenPart}
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.utils.EthTxGenerator.Arg.Integer
import com.wavesplatform.transaction.utils.{EthTxGenerator, Signed}
import com.wavesplatform.transaction.{Asset, EthereumTransaction, GenesisTransaction}
import com.wavesplatform.utils.{EthHelpers, EthSetChainId}

class EthereumInvokeTest extends PropSpec with WithDomain with EthHelpers with EthSetChainId {
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

  private def makeDAppScript(assets: Seq[Asset], syncDAppAddress: Address, version: StdLibVersion, syncCall: Boolean) =
    TestCompiler(version).compileContract {
      val payments = assets.map(a => s""" AttachedPayment(base58'$a', $paymentAmount) """).mkString(", ")
      s"""
         | @Callable(i)
         | func default(value: Int) = {
         |   ${if (syncCall) s""" strict r = invoke(Address(base58'$syncDAppAddress'), "default", [], []) """ else ""}
         |   let check =
         |     value == $passingArg &&
         |     ${if (version > V3) s"i.payments == [$payments]" else s"i.payment == ${if (payments.nonEmpty) payments else "unit"}"}
         |
         |   ${if (version > V3) """[ BooleanEntry("check", check) ]""" else """ WriteSet([DataEntry("check", check)]) """}
         | }
     """.stripMargin
    }

  private def makeSyncDApp(
      version: StdLibVersion,
      fee: Long,
      thisDApp: KeyPair,
      callerDAppPk: PublicKey,
      invoker: Address,
      invokerPk: PublicKey
  ): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   let check =
         |     this                    == Address(base58'${thisDApp.toAddress}')     &&
         |     i.caller                == Address(base58'${callerDAppPk.toAddress}') &&
         |     i.originCaller          == Address(base58'$invoker')                  &&
         |     i.callerPublicKey       == base58'$callerDAppPk'                      &&
         |     i.originCallerPublicKey == base58'$invokerPk'                         &&
         |     i.fee                   == $fee                                       &&
         |     i.payments              == []                                         &&
         |     i.feeAssetId            == unit
         |   ${if (version > V3) """[ BooleanEntry("check", check) ]""" else """ WriteSet([DataEntry("check", check)]) """}
         | }
       """.stripMargin
    )

  private def preconditions(
      dAppVersion: StdLibVersion,
      assetScriptVersion: StdLibVersion,
      paymentCount: Int,
      syncCall: Boolean,
      invokeExpression: Boolean
  ) = {
    val fee       = ciFee().sample.get
    val dApp      = accountGen.sample.get
    val syncDApp  = accountGen.sample.get
    val eCKeyPair = ethAccountGen.sample.get

    val dAppAddress     = dApp.toAddress
    val syncDAppAddress = syncDApp.toAddress
    val emptyScript     = Some(ExprScript(Terms.TRUE).explicitGet())

    val issues = (1 to paymentCount).map(_ => Signed.issue(2.toByte, dApp, "Asset", "", ENOUGH_AMT, 8, true, emptyScript, fee, ts))
    val assets = issues.map(i => IssuedAsset(i.id()))

    val dAppScript = makeDAppScript(assets, syncDAppAddress, dAppVersion, syncCall)
    val setDApp    = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(dAppScript), fee, ts).explicitGet()

    val ethInvoke =
      if (invokeExpression)
        toEthInvokeExpression(setDApp, eCKeyPair, Some(FUNCTION_CALL(User("default"), List(CONST_LONG(passingArg)))))
      else
        EthTxGenerator.generateEthInvoke(eCKeyPair, dAppAddress, "default", Seq(Integer(passingArg)), assets.map(a => Payment(paymentAmount, a)))

    val invoker   = ethInvoke.senderAddress()
    val invokerPk = ethInvoke.signerPublicKey()

    val setScriptAndTransferAssets = assets.flatMap { asset =>
      val script    = assetScript(ethInvoke, dAppAddress, assets, asset, assetScriptVersion)
      val setScript = SetAssetScriptTransaction.selfSigned(1.toByte, dApp, asset, Some(script), fee, ts).explicitGet()
      val transfer  = TransferTransaction.selfSigned(2.toByte, dApp, invoker, asset, ENOUGH_AMT, Waves, fee, ByteStr.empty, ts).explicitGet()
      List(setScript, transfer)
    }

    val callerPk        = if (invokeExpression) invokerPk else dApp.publicKey
    val syncDAppVersion = if (dAppVersion >= V5) dAppVersion else V5
    val syncDAppScript  = makeSyncDApp(syncDAppVersion, ethInvoke.assetFee._2, syncDApp, callerPk, invoker, invokerPk)
    val setSyncDApp     = SetScriptTransaction.selfSigned(1.toByte, syncDApp, Some(syncDAppScript), fee, ts).explicitGet()

    val gTxs = Seq(invoker, dAppAddress, syncDAppAddress).map(a => GenesisTransaction.create(a, ENOUGH_AMT, ts).explicitGet())
    (gTxs ++ List(setDApp, setSyncDApp) ++ issues ++ setScriptAndTransferAssets, ethInvoke, dAppAddress, syncDAppAddress, assets)
  }

  private def assert(
      dAppVersion: StdLibVersion,
      assetScriptVersion: StdLibVersion,
      paymentCount: Int,
      syncCall: Boolean = false,
      invokeExpression: Boolean = false
  ) = {
    val (preparingTxs, ethInvoke, dApp, syncDApp, assets) = preconditions(dAppVersion, assetScriptVersion, paymentCount, syncCall, invokeExpression)

    val ethSender   = ethInvoke.senderAddress()
    val dataAddress = if (invokeExpression) ethSender else dApp

    withDomain(RideV6) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(ethInvoke)

      d.liquidDiff.errorMessage(ethInvoke.id()) shouldBe None
      d.liquidDiff.accountData(dataAddress).data("check").value shouldBe true
      if (syncCall) d.liquidDiff.accountData(syncDApp).data("check").value shouldBe true

      val assetsPortfolio = assets.map(Portfolio.build(_, paymentAmount)).fold(Portfolio())(_ |+| _)
      d.liquidDiff.portfolios.getOrElse(dApp, Portfolio()) shouldBe assetsPortfolio
      d.liquidDiff.portfolios(ethSender) shouldBe Portfolio(-ethInvoke.assetFee._2).minus(assetsPortfolio)
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
      assert(dAppVersion = v, assetScriptVersion = v, 0, syncCall = v >= V5, invokeExpression = true)
    }
  }
}
