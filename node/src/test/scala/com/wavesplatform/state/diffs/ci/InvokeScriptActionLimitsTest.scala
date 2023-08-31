package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.*
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.ContractLimits.*
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{utils as _, *}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import org.scalatest.{Assertion, EitherValues}
import org.scalatest.matchers.Matcher

class InvokeScriptActionLimitsTest extends PropSpec with WithDomain with DBCacheSettings with EitherValues {

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

    property(s"NODE-741. Allow not more $limit ScriptTransfer/Lease/LeaseCancel actions for V${version.id}") {
      val wavesTransferActionsCount = (limit - 8) / 2
      val (preparingTxs1, invoke1, masterAddress, serviceAddress1) =
        scenario(
          balanceActionsWithIssueMasterContract(_, _, wavesTransferActionsCount, version),
          balanceActionsWithIssueServiceContract(version, wavesTransferActionsCount)
        )

      assertDiffAndState(Seq(TestBlock.create(preparingTxs1)), TestBlock.create(Seq(invoke1), Block.ProtoBlockVersion), features(version)) {
        case (diff, bc) =>
          diff.scriptResults(invoke1.id()).error shouldBe None
          bc.accountData(masterAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
          bc.accountData(serviceAddress1, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
      }

      val (preparingTxs2, invoke2, _, _) =
        scenario(
          balanceActionsWithIssueMasterContract(_, _, wavesTransferActionsCount, version, extraAction = true),
          balanceActionsWithIssueServiceContract(version, wavesTransferActionsCount)
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

    property(s"NODE-740. Allow not more $limit Issue/Reissue/Burn/SponsorFee actions for V${version.id}") {
      val actionsCount = limit / 2
      val (preparingTxs1, invoke1, masterAddress, serviceAddress1) =
        scenario(
          (_: Address, alias: Alias) => assetActionsMasterContract(alias, actionsCount, version),
          assetAndDataActionsServiceContract(version, actionsCount)
        )

      assertDiffAndState(Seq(TestBlock.create(preparingTxs1)), TestBlock.create(Seq(invoke1), Block.ProtoBlockVersion), features(version)) {
        case (diff, bc) =>
          diff.scriptResults(invoke1.id()).error shouldBe None
          bc.accountData(masterAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
          bc.accountData(serviceAddress1, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
      }

      val (preparingTxs2, invoke2, _, _) =
        scenario(
          (_: Address, alias: Alias) => assetActionsMasterContract(alias, actionsCount, version, extraAction = true),
          assetAndDataActionsServiceContract(version, actionsCount)
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
    s"""NODE-742. Allow ${ContractLimits.MaxBalanceScriptActionsAmountV6} ScriptTransfer/Lease/LeaseCancel,
       |${ContractLimits.MaxAssetScriptActionsAmountV6} Issue/Reissue/Burn/SponsorFee 
       |and ${ContractLimits.MaxWriteSetSize} data actions for V6""".stripMargin
  ) {
    val (preparingTxs, invoke, masterAddress, serviceAddress) =
      scenario(
        (_: Address, alias: Alias) =>
          balanceAndDataActionsMasterContract(alias, ContractLimits.MaxBalanceScriptActionsAmountV6, ContractLimits.MaxWriteSetSize / 2, V6),
        assetAndDataActionsServiceContract(V6, ContractLimits.MaxAssetScriptActionsAmountV6, ContractLimits.MaxWriteSetSize / 2)
      )

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invoke), Block.ProtoBlockVersion), features(V6)) { case (diff, bc) =>
      diff.scriptResults(invoke.id()).error shouldBe None
      bc.accountData(masterAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
      bc.accountData(serviceAddress, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
    }
  }

  property(
    s"Balance and asset action limits is determined by current invoke version before ${BlockchainFeatures.BlockRewardDistribution.description} activation"
  ) {
    val settings = DomainPresets.RideV6

    checkBalanceActionsLimits(V5, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkBalanceActionsLimits(V5, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkBalanceActionsLimits(V6, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkBalanceActionsLimits(V6, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = beRight,
      moreV5MoreV6Result = produce("ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"),
      moreV5LessV6Result = beRight
    )

    checkAssetActionsLimits(V5, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkAssetActionsLimits(V5, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Issue, Reissue, Burn, SponsorFee actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkAssetActionsLimits(V6, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkAssetActionsLimits(V6, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = beRight,
      moreV5MoreV6Result = produce("Issue, Reissue, Burn, SponsorFee actions count limit is exceeded"),
      moreV5LessV6Result = beRight
    )
  }

  property(
    s"Balance and asset action limits is determined by root invoke version after ${BlockchainFeatures.BlockRewardDistribution.description} activation"
  ) {
    val settings = DomainPresets.BlockRewardDistribution

    checkBalanceActionsLimits(V5, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkBalanceActionsLimits(V5, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkBalanceActionsLimits(V6, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = beRight,
      moreV5MoreV6Result = produce("ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"),
      moreV5LessV6Result = beRight
    )
    checkBalanceActionsLimits(V6, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = beRight,
      moreV5MoreV6Result = produce("ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"),
      moreV5LessV6Result = beRight
    )

    checkAssetActionsLimits(V5, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkAssetActionsLimits(V5, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = produce("Actions count limit is exceeded"),
      moreV5MoreV6Result = produce("Actions count limit is exceeded"),
      moreV5LessV6Result = produce("Actions count limit is exceeded")
    )
    checkAssetActionsLimits(V6, V5, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = beRight,
      moreV5MoreV6Result = produce("Issue, Reissue, Burn, SponsorFee actions count limit is exceeded"),
      moreV5LessV6Result = beRight
    )
    checkAssetActionsLimits(V6, V6, settings)(
      equalV5LessV6Result = beRight,
      moreV5EqualV6Result = beRight,
      moreV5MoreV6Result = produce("Issue, Reissue, Burn, SponsorFee actions count limit is exceeded"),
      moreV5LessV6Result = beRight
    )
  }

  private def checkBalanceActionsLimits(rootVersion: StdLibVersion, nestedVersion: StdLibVersion, settings: WavesSettings)(
      equalV5LessV6Result: Matcher[Either[?, ?]],
      moreV5EqualV6Result: Matcher[Either[?, ?]],
      moreV5MoreV6Result: Matcher[Either[?, ?]],
      moreV5LessV6Result: Matcher[Either[?, ?]]
  ): Assertion = {
    val master           = TxHelpers.signer(0)
    val invoker          = TxHelpers.signer(1)
    val allActionsNested = TxHelpers.signer(2)
    val lastNested       = TxHelpers.signer(3)

    val fee = 10.waves

    withDomain(settings, balances = AddrWithBalance.enoughBalances(master, invoker, allActionsNested, lastNested)) { d =>
      val setMasterScript           = TxHelpers.setScript(master, masterContract(rootVersion, allActionsNested.toAddress, lastNested.toAddress))
      val setAllActionsNestedScript = TxHelpers.setScript(allActionsNested, allActionsNestedContract(nestedVersion))
      val setLastNestedScript = TxHelpers.setScript(lastNested, balanceActionsNestedContract(nestedVersion, MaxCallableActionsAmountBeforeV6(V5) - 9))
      val invoke              = () => TxHelpers.invoke(master.toAddress, Some("foo"), invoker = invoker, fee = fee)

      // non-data actions count == limit for V5, balance actions count < limit for V6
      d.appendBlock(setMasterScript, setAllActionsNestedScript, setLastNestedScript)
      d.appendBlockE(invoke()) should equalV5LessV6Result

      // non-data actions count > limit for V5, balance actions count == limit for V6
      d.appendBlock(TxHelpers.setScript(lastNested, balanceActionsNestedContract(nestedVersion, MaxBalanceScriptActionsAmountV6 - 5)))
      d.appendBlockE(invoke()) should moreV5EqualV6Result

      // non-data actions count > limit for V5, balance actions count > limit for V6
      d.appendBlock(TxHelpers.setScript(lastNested, balanceActionsNestedContract(nestedVersion, MaxBalanceScriptActionsAmountV6 - 5 + 1)))
      d.appendBlockE(invoke()) should moreV5MoreV6Result

      // non-data actions count > limit for V5, balance actions count < limit for V6
      d.appendBlock(TxHelpers.setScript(lastNested, balanceActionsNestedContract(nestedVersion, MaxCallableActionsAmountBeforeV6(V5) - 9 + 1)))
      d.appendBlockE(invoke()) should moreV5LessV6Result
    }
  }

  private def checkAssetActionsLimits(rootVersion: StdLibVersion, nestedVersion: StdLibVersion, settings: WavesSettings)(
      equalV5LessV6Result: Matcher[Either[?, ?]],
      moreV5EqualV6Result: Matcher[Either[?, ?]],
      moreV5MoreV6Result: Matcher[Either[?, ?]],
      moreV5LessV6Result: Matcher[Either[?, ?]]
  ): Assertion = {
    val master           = TxHelpers.signer(0)
    val invoker          = TxHelpers.signer(1)
    val allActionsNested = TxHelpers.signer(2)
    val lastNested       = TxHelpers.signer(3)

    val fee = 10.waves

    withDomain(settings, balances = AddrWithBalance.enoughBalances(master, invoker, allActionsNested, lastNested)) { d =>
      val setMasterScript           = TxHelpers.setScript(master, masterContract(rootVersion, allActionsNested.toAddress, lastNested.toAddress))
      val setAllActionsNestedScript = TxHelpers.setScript(allActionsNested, allActionsNestedContract(nestedVersion))
      val setLastNestedScript = TxHelpers.setScript(lastNested, assetActionsNestedContract(nestedVersion, MaxCallableActionsAmountBeforeV6(V5) - 10))
      val invoke              = () => TxHelpers.invoke(master.toAddress, Some("foo"), invoker = invoker, fee = fee)

      // non-data actions count == limit for V5, asset actions count < limit for V6
      d.appendBlock(setMasterScript, setAllActionsNestedScript, setLastNestedScript)
      d.appendBlockE(invoke()) should equalV5LessV6Result

      // non-data actions count > limit for V5, asset actions count == limit for V6
      d.appendBlock(TxHelpers.setScript(lastNested, assetActionsNestedContract(nestedVersion, MaxAssetScriptActionsAmountV6 - 7)))
      d.appendBlockE(invoke()) should moreV5EqualV6Result

      // non-data actions count > limit for V5, asset actions count > limit for V6
      d.appendBlock(TxHelpers.setScript(lastNested, assetActionsNestedContract(nestedVersion, MaxAssetScriptActionsAmountV6 - 7 + 1)))
      d.appendBlockE(invoke()) should moreV5MoreV6Result

      // non-data actions count > limit for V5, asset actions count < limit for V6
      d.appendBlock(TxHelpers.setScript(lastNested, assetActionsNestedContract(nestedVersion, MaxCallableActionsAmountBeforeV6(V5) - 10 + 1)))
      d.appendBlockE(invoke()) should moreV5LessV6Result
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

  private def balanceActionsNestedContract(version: StdLibVersion, transferActionsCount: Int): Script = {
    val recipient = TxHelpers.address(100)
    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         |@Callable(i)
         |func bar() = {
         | let l = Lease(Address(base58'$recipient'), 2)
         | [
         |   ${s"ScriptTransfer(Address(base58'$recipient'), 1, unit), " * transferActionsCount}
         |   l,
         |   LeaseCancel(l.calculateLeaseId())
         | ]
         |}
         |""".stripMargin
    TestCompiler(version).compileContract(script)
  }

  private def assetActionsNestedContract(version: StdLibVersion, reissueActionsCount: Int): Script = {
    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         |@Callable(i)
         |func bar() = {
         | let issue = Issue("newAsset", "desc", 100, 0, true)
         | [
         |   issue,
         |   Burn(issue.calculateAssetId(), 10),
         |   SponsorFee(issue.calculateAssetId(), 2),
         |   ${(1 to reissueActionsCount).map(idx => s"Reissue(issue.calculateAssetId(), $idx, true)").mkString(",")}
         | ]
         |}
         |""".stripMargin
    TestCompiler(version).compileContract(script)
  }

  private def allActionsNestedContract(version: StdLibVersion): Script = {
    val recipient = TxHelpers.address(100)
    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         |@Callable(i)
         |func bar() = {
         | let l = Lease(Address(base58'$recipient'), 1)
         | let issue = Issue("test", "test", 100, 0, true)
         | [
         |   BinaryEntry("bin", base58''),
         |   BooleanEntry("bool", true),
         |   DeleteEntry("bin"),
         |   IntegerEntry("int", 1),
         |   StringEntry("str", "str"),
         |   issue,
         |   Burn(issue.calculateAssetId(), 10),
         |   Reissue(issue.calculateAssetId(), 10, true),
         |   ScriptTransfer(Address(base58'$recipient'), 1, unit),
         |   l,
         |   LeaseCancel(l.calculateLeaseId()),
         |   SponsorFee(issue.calculateAssetId(), 2)
         | ]
         |}
         |""".stripMargin
    TestCompiler(version).compileContract(script)
  }

  private def masterContract(version: StdLibVersion, allActionsServiceDApp: Address, serviceDApp: Address): Script = {
    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         |@Callable(i)
         |func foo() = {
         | strict inv1 = invoke(Address(base58'$allActionsServiceDApp'), "bar", [], [])
         | strict inv2 = invoke(Address(base58'$serviceDApp'), "bar", [], [])
         | []
         |}
         |""".stripMargin
    TestCompiler(version).compileContract(script)
  }

  private def balanceActionsWithIssueServiceContract(version: StdLibVersion, wavesTransferActionsCount: Int): Script = {
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
         |       ${"ScriptTransfer(Address(a), 1, unit), " * wavesTransferActionsCount}
         |       BinaryEntry("asset", n.calculateAssetId()),
         |       n,
         |       ScriptTransfer(Address(a), 1, n.calculateAssetId())], ${wavesTransferActionsCount + 4})
         |   else
         |     throw("Bad caller")
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  private def balanceActionsWithIssueMasterContract(
      otherAcc: Address,
      alias: Alias,
      wavesTransferActionsCount: Int,
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
         |    strict r = invoke(Alias("${alias.name}"), "bar", [this.bytes, "aaaaaaaa"], [AttachedPayment(unit, ${wavesTransferActionsCount + 4})])
         |    strict r1 = invoke(Alias("${alias.name}"), "bar", [this.bytes, "bbbbbbbb"], [AttachedPayment(unit, ${wavesTransferActionsCount + 4})])
         |    let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
         |    let b2 = wavesBalance(this)
         |    let ob2 = wavesBalance(Address(base58'$otherAcc'))
         |    let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
         |    if data == 1
         |    then
         |      if ob1.regular+8 == ob2.regular && b1.regular == b2.regular+8 && ab == 1
         |      then
         |        let l = Lease(Address(base58'$otherAcc'), 23)
         |        [
         |          IntegerEntry("key", 1),
         |          Lease(Address(base58'$otherAcc'), 13),
         |          Lease(Address(base58'$otherAcc'), 15),
         |          $additionalBalanceActionsForV6
         |          $extraLeaseAction
         |          l,
         |          LeaseCancel(l.calculateLeaseId())
         |        ]
         |      else
         |        throw("Balance check failed")
         |    else
         |      throw("Bad state")
         |  else
         |   throw("Imposible")
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  private def assetActionsMasterContract(
      alias: Alias,
      actionsCount: Int,
      version: StdLibVersion,
      extraAction: Boolean = false
  ): Script = {
    val extraIssueAction = if (extraAction) s"""Issue("extraAsset", "", 100, 8, true, unit, ${actionsCount + 1}),""" else ""

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
         |     ${(0 until actionsCount).map(ind => s"""Issue("masterAsset$ind", "", 100, 8, true, unit, $ind)""").mkString(",\n")}
         |   ]
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  private def assetAndDataActionsServiceContract(version: StdLibVersion, assetActionsCount: Int, dataActionsCount: Int = 2): Script = {
    val script =
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         | @Callable(i)
         | func bar() = {
         |   let issue = Issue("serviceAsset", "", 100, 8, true, unit, 0)
         |   let assetId = issue.calculateAssetId()
         |   [
         |     issue,
         |     IntegerEntry("bar", 1),
         |     ${(0 until (assetActionsCount - 4)).map(ind => s"""Issue("serviceAsset$ind", "", 100, 8, true, unit, $ind)""").mkString(",\n")},
         |     ${(0 until (dataActionsCount - 1)).map(ind => s"""IntegerEntry("bar$ind", 1)""").mkString(",\n")},
         |     Reissue(assetId, 100, true),
         |     Burn(assetId, 50),
         |     SponsorFee(assetId, 2)
         |   ]
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }

  private def balanceAndDataActionsMasterContract(alias: Alias, balanceActionsCount: Int, dataActionsCount: Int, version: StdLibVersion): Script = {

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
         |     BinaryEntry("bin", l.calculateLeaseId()),
         |     BooleanEntry("bool", true),
         |     DeleteEntry("bool"),
         |     StringEntry("str", "string"),
         |     ${(0 until (dataActionsCount - 5)).map(ind => s"""IntegerEntry("key$ind", 1)""").mkString(",\n")},
         |     ${(0 until (balanceActionsCount - 3)).map(_ => s"""ScriptTransfer(Alias("${alias.name}"), 1, unit)""").mkString(",\n")}
         |   ]
         | }
         |""".stripMargin

    TestCompiler(version).compileContract(script)
  }
}
