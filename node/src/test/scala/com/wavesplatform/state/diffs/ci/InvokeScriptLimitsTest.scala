package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.*
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{utils as _, *}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import org.scalatest.EitherValues

class InvokeScriptLimitsTest extends PropSpec with WithState with DBCacheSettings with EitherValues {

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
    Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> 0
    )
  )

  private val fsWithV6 = fsWithV5.copy(
    preActivatedFeatures = fsWithV5.preActivatedFeatures.updated(BlockchainFeatures.RideV6.id, 0)
  )

  Seq(V5, V6).foreach { version =>
    val limit =
      if (version == V6)
        ContractLimits.MaxBalanceScriptActionsAmountV6
      else
        ContractLimits.MaxCallableActionsAmountBeforeV6(version)

    property(s"Allow not more $limit ScriptTransfer/Lease/LeaseCancel actions for V${version.id}") {
      val wavesTransferAmount = (limit - 8) / 2
      val (preparingTxs1, invoke1, masterAddress, serviceAddress1) =
        scenario(
          balanceActionsWithIssueMasterContract(_, _, wavesTransferAmount, version),
          balanceActionsServiceContract(version, wavesTransferAmount)
        )

      assertDiffAndState(Seq(TestBlock.create(preparingTxs1)), TestBlock.create(Seq(invoke1), Block.ProtoBlockVersion), features(version)) {
        case (diff, bc) =>
          diff.scriptResults(invoke1.id()).error shouldBe None
          bc.accountData(masterAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
          bc.accountData(serviceAddress1, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
      }

      val (preparingTxs2, invoke2, _, _) =
        scenario(
          balanceActionsWithIssueMasterContract(_, _, wavesTransferAmount, version, extraAction = true),
          balanceActionsServiceContract(version, wavesTransferAmount)
        )

      val errMsg =
        if (version == V6)
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"
        else
          "Actions count limit is exceeded"

      assertDiffEi(Seq(TestBlock.create(preparingTxs2)), TestBlock.create(Seq(invoke2), Block.ProtoBlockVersion), features(version)) { ei =>
        ei should produceRejectOrFailedDiff(errMsg)
      }
    }
  }

  Seq(V5, V6).foreach { version =>
    val limit =
      if (version == V6)
        ContractLimits.MaxAssetScriptActionsAmountV6
      else
        ContractLimits.MaxCallableActionsAmountBeforeV6(version)

    property(s"Allow not more $limit Issue/Reissue/Burn/SponsorFee actions for V${version.id}") {
      val actionsAmount = limit / 2
      val (preparingTxs1, invoke1, masterAddress, serviceAddress1) =
        scenario(
          (_: Address, alias: Alias) => assetActionsMasterContract(alias, actionsAmount, version),
          assetActionsServiceContract(version, actionsAmount)
        )

      assertDiffAndState(Seq(TestBlock.create(preparingTxs1)), TestBlock.create(Seq(invoke1), Block.ProtoBlockVersion), features(version)) {
        case (diff, bc) =>
          diff.scriptResults(invoke1.id()).error shouldBe None
          bc.accountData(masterAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
          bc.accountData(serviceAddress1, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
      }

      val (preparingTxs2, invoke2, _, _) =
        scenario(
          (_: Address, alias: Alias) => assetActionsMasterContract(alias, actionsAmount, version, extraAction = true),
          assetActionsServiceContract(version, actionsAmount)
        )

      val errMsg =
        if (version == V6)
          "Issue, Reissue, Burn, SponsorFee actions count limit is exceeded"
        else
          "Actions count limit is exceeded"

      assertDiffEi(Seq(TestBlock.create(preparingTxs2)), TestBlock.create(Seq(invoke2), Block.ProtoBlockVersion), features(version)) { ei =>
        ei should produceRejectOrFailedDiff(errMsg)
      }
    }
  }

  property(
    s"""Allow ${ContractLimits.MaxBalanceScriptActionsAmountV6} ScriptTransfer/Lease/LeaseCancel 
       |and ${ContractLimits.MaxAssetScriptActionsAmountV6} Issue/Reissue/Burn/SponsorFee actions for V6""".stripMargin
  ) {
    val (preparingTxs, invoke, masterAddress, serviceAddress) =
      scenario(
        (_: Address, alias: Alias) => onlyBalanceActionsMasterContract(alias, ContractLimits.MaxBalanceScriptActionsAmountV6, V6),
        assetActionsServiceContract(V6, ContractLimits.MaxAssetScriptActionsAmountV6)
      )

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features(V6)) { case (diff, bc) =>
      diff.scriptResults(invoke.id()).error shouldBe None
      bc.accountData(masterAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(serviceAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  private def scenario(masterDApp: (Address, Alias) => Script, serviceDApp: Script): (Seq[Transaction], InvokeScriptTransaction, Address, Address) = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val fee = 100.waves

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress),
      TxHelpers.genesis(service.toAddress)
    )
    val alias            = Alias.create("alias").explicitGet()
    val aliasTx          = TxHelpers.createAlias(alias.name, service, fee)
    val setMasterScript  = TxHelpers.setScript(master, masterDApp(service.toAddress, alias), fee)
    val setServiceScript = TxHelpers.setScript(service, serviceDApp, fee)
    val preparingTxs     = genesis :+ aliasTx :+ setMasterScript :+ setServiceScript

    val invoke = TxHelpers.invoke(master.toAddress, func = Some("foo"), invoker = invoker, payments = List(Payment(10L, Waves)), fee = fee)

    (preparingTxs, invoke, master.toAddress, service.toAddress)
  }

  private def features(version: StdLibVersion): FunctionalitySettings =
    version match {
      case V5 => fsWithV5
      case _  => fsWithV6
    }

  private def balanceActionsServiceContract(version: StdLibVersion, wavesTransferAmount: Int): Script = {
    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         | @Callable(i)
         | func bar(a: ByteVector, an: String) = {
         |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
         |   then
         |     let n = Issue(an, an, 1, 0, false, unit, 0)
         |     ([
         |       IntegerEntry("bar", 1),
         |       ${"ScriptTransfer(Address(a), 1, unit), " * wavesTransferAmount}
         |       BinaryEntry("asset", n.calculateAssetId()),
         |       n,
         |       ScriptTransfer(Address(a), 1, n.calculateAssetId())], ${wavesTransferAmount + 4})
         |   else
         |     throw("Bad caller")
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  def balanceActionsWithIssueMasterContract(
      otherAcc: Address,
      alias: Alias,
      wavesTransferAmount: Int,
      version: StdLibVersion,
      extraAction: Boolean = false
  ): Script = {
    val additionalBalanceActionsForV6 =
      if (version == V6)
        s"""
           |Lease(Address(base58'$otherAcc'), 1),
           |Lease(Address(base58'$otherAcc'), 2),
           |""".stripMargin
      else
        ""
    val extraLeaseAction = if (extraAction) s"Lease(Address(base58'$otherAcc'), 16)," else ""

    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         | @Callable(i)
         | func foo() = {
         |  let b1 = wavesBalance(this)
         |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
         |  if b1 == b1 && ob1 == ob1
         |  then
         |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes, "aaaaaaaa"], [AttachedPayment(unit, ${wavesTransferAmount + 4})])
         |    let r1 = invoke(Alias("${alias.name}"), "bar", [this.bytes, "bbbbbbbb"], [AttachedPayment(unit, ${wavesTransferAmount + 4})])
         |    if r == r1
         |    then
         |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
         |     let b2 = wavesBalance(this)
         |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
         |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
         |     if data == 1
         |     then
         |      if ob1.regular+8 == ob2.regular && b1.regular == b2.regular+8 && ab == 1
         |      then
         |       let l = Lease(Address(base58'$otherAcc'), 23)
         |       [
         |        IntegerEntry("key", 1),
         |        Lease(Address(base58'$otherAcc'), 13),
         |        Lease(Address(base58'$otherAcc'), 15),
         |        $additionalBalanceActionsForV6
         |        $extraLeaseAction
         |        l,
         |        LeaseCancel(l.calculateLeaseId())
         |       ]
         |      else
         |       throw("Balance check failed")
         |    else
         |     throw("Bad state")
         |   else
         |    throw("Bad returned value")
         |  else
         |   throw("Imposible")
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  def assetActionsMasterContract(
      alias: Alias,
      actionsAmount: Int,
      version: StdLibVersion,
      extraAction: Boolean = false
  ): Script = {
    val extraIssueAction = if (extraAction) s"""Issue("extraAsset", "", 100, 8, true, unit, ${actionsAmount + 1}),""" else ""

    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         | @Callable(i)
         | func foo() = {
         |   strict r = invoke(Alias("${alias.name}"), "bar", [], [])
         |   [
         |     IntegerEntry("key", 1),
         |     $extraIssueAction
         |     ${(0 until actionsAmount).map(ind => s"""Issue("masterAsset$ind", "", 100, 8, true, unit, $ind)""").mkString(",\n")}
         |   ]
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  private def assetActionsServiceContract(version: StdLibVersion, actionsAmount: Int): Script = {
    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         | @Callable(i)
         | func bar() = {
         |   let issue = Issue("serviceAsset0", "", 100, 8, true, unit, 0)
         |   let assetId = issue.calculateAssetId()
         |   [
         |     issue,
         |     IntegerEntry("bar", 1),
         |     ${(1 until (actionsAmount - 3)).map(ind => s"""Issue("serviceAsset$ind", "", 100, 8, true, unit, $ind)""").mkString(",\n")},
         |     Reissue(assetId, 100, true),
         |     Burn(assetId, 50),
         |     SponsorFee(assetId, 2)
         |   ]
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  private def onlyBalanceActionsMasterContract(alias: Alias, actionsAmount: Int, version: StdLibVersion): Script = {

    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         | @Callable(i)
         | func foo() = {
         |   strict r = invoke(Alias("${alias.name}"), "bar", [], [])
         |   let l = Lease(Alias("${alias.name}"), 10)
         |   [
         |     IntegerEntry("key", 1),
         |     l,
         |     Lease(Alias("${alias.name}"), 20),
         |     LeaseCancel(l.calculateLeaseId()),
         |     ${(0 until (actionsAmount - 3)).map(_ => s"""ScriptTransfer(Alias("${alias.name}"), 1, unit)""").mkString(",\n")}
         |   ]
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }
}
