package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.state.diffs.*
import com.wavesplatform.state.diffs.FeeValidation.FeeConstants
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state.{EmptyDataEntry, SponsorshipValue}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.{RideV4, RideV6}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TransactionType, TxHelpers}
import org.scalatest.EitherValues

class CallableV4DiffTest extends PropSpec with WithDomain with EitherValues {

  private val features = RideV6.blockchainSettings.functionalitySettings

  property("reissue and burn actions result state") {
    val (genesis, setScript, invoke, issue, master, reissueAmount, burnAmount) = paymentPreconditions(0.005.waves, fail = false)
    assertDiffAndState(
      Seq(TestBlock.create(genesis :+ setScript :+ issue)),
      TestBlock.create(Seq(invoke)),
      features
    ) { case (_, blockchain) =>
      val asset        = IssuedAsset(issue.id())
      val resultAmount = issue.quantity.value + reissueAmount - burnAmount

      blockchain.assetDescription(asset).get.totalVolume shouldBe resultAmount
      blockchain.balance(master.toAddress, asset) shouldBe resultAmount
    }
  }

  property("asset script can allow burn and reissue") {
    val allowBurnAndReissueAsset =
      assetVerifier(
        """
          | match tx {
          |   case _: ReissueTransaction | BurnTransaction => true
          |   case _ => false
          | }
        """.stripMargin
      )

    val (genesis, setScript, invoke, issue, _, _, _) = paymentPreconditions(0.005.waves, fail = false, Some(allowBurnAndReissueAsset))
    assertDiffEi(
      Seq(TestBlock.create(genesis :+ setScript :+ issue)),
      TestBlock.create(Seq(invoke)),
      features
    )(_.explicitGet())
  }

  property("action state changes affects subsequent actions") {
    val (genesis, setScript, invoke, issue, master, invoker, reissueAmount, burnAmount, transferAmount) =
      multiActionPreconditions(invokeFee = 0.029.waves, withScriptError = false)
    assertDiffAndState(
      Seq(TestBlock.create(genesis :+ setScript :+ issue)),
      TestBlock.create(Seq(invoke)),
      features
    ) { case (_, blockchain) =>
      val asset                 = IssuedAsset(issue.id())
      val totalResultAmount     = issue.quantity.value + (reissueAmount - burnAmount) * 2
      val issuerResultAmount    = issue.quantity.value + (reissueAmount - burnAmount - transferAmount) * 2
      val recipientResultAmount = transferAmount * 2

      blockchain.assetDescription(asset).get.totalVolume shouldBe totalResultAmount
      blockchain.balance(master.toAddress, asset) shouldBe issuerResultAmount
      blockchain.balance(invoker.toAddress, asset) shouldBe recipientResultAmount
    }
  }

  property("check fee before activation of SynchronousCalls") {
    val minimalFee                                         = 6 * ScriptExtraFee + FeeConstants(TransactionType.InvokeScript) * FeeValidation.FeeUnit
    val (genesis, setScript, invoke, issue, _, _, _, _, _) = multiActionPreconditions(invokeFee = 0.005.waves, withScriptError = false)
    assertDiffEi(
      Seq(TestBlock.create(genesis :+ setScript :+ issue)),
      TestBlock.create(Seq(invoke)),
      RideV4.blockchainSettings.functionalitySettings
    )(_ should produceRejectOrFailedDiff(s" with 6 total scripts invoked does not exceed minimal value of $minimalFee WAVES"))
  }

  ignore("trace") {
    val (genesis, setScript, invoke, issue, _, _, _, _, _) = multiActionPreconditions(invokeFee = 0.005.waves, withScriptError = true)
    assertDiffEiTraced(
      Seq(TestBlock.create(genesis :+ setScript :+ issue)),
      TestBlock.create(Seq(invoke)),
      features
    ) { r =>
      r.trace.size shouldBe 4
      r.trace.head.asInstanceOf[InvokeScriptTrace].resultE.explicitGet()

      val assetTrace = r.trace.tail.asInstanceOf[List[AssetVerifierTrace]]
      assetTrace.take(2).foreach(_.errorOpt shouldBe None)
      assetTrace.last.errorOpt.get shouldBe r.resultE.left.value.asInstanceOf[TransactionValidationError].cause
    }
  }

  property("diff contains delete entries") {
    val deleteEntryDApp = dApp(
      """
        | [
        |   DeleteEntry("key1"),
        |   DeleteEntry("key2")
        | ]
        |
        |""".stripMargin
    )

    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val setScript = TxHelpers.setScript(master, deleteEntryDApp)
    val invoke    = TxHelpers.invoke(master.toAddress, func = None, invoker = invoker)

    assertDiffAndState(
      Seq(TestBlock.create(genesis :+ setScript)),
      TestBlock.create(Seq(invoke)),
      features
    ) { case (diff, _) =>
      diff.accountData(master.toAddress) shouldBe
        Map(
          "key1" -> EmptyDataEntry("key1"),
          "key2" -> EmptyDataEntry("key2")
        )
    }
  }

  private def paymentPreconditions(
      invokeFee: Long,
      fail: Boolean,
      assetScript: Option[Script] = None
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction, KeyPair, Long, Long) = {
    val master        = TxHelpers.signer(0)
    val invoker       = TxHelpers.signer(1)
    val reissueAmount = 10
    val burnAmount    = 5

    val genesis = List(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val issue     = TxHelpers.issue(master, 100, script = assetScript)
    val setScript = TxHelpers.setScript(master, reissueAndBurnDApp(issue.id(), reissueAmount, burnAmount, fail))
    val invoke    = TxHelpers.invoke(master.toAddress, func = None, invoker = invoker, fee = invokeFee)
    (genesis, setScript, invoke, issue, master, reissueAmount, burnAmount)
  }

  private def sponsorFeePreconditions(invokeFee: Long): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, Long) = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val minSponsoredAssetFee = 1000L

    val genesis = List(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val setScript = TxHelpers.setScript(master, sponsorFeeDApp(Some(minSponsoredAssetFee)))
    val invoke    = TxHelpers.invoke(master.toAddress, func = None, invoker = invoker, fee = invokeFee)

    (genesis, setScript, invoke, minSponsoredAssetFee)
  }

  private def multiActionDApp(
      assetId: ByteStr,
      recipient: Address,
      reissueAmount: Long,
      burnAmount: Long,
      transferAmount: Long
  ): Script =
    dApp(
      s"""
         | [
         |   IntegerEntry("int", 1),
         |   BooleanEntry("bool", true),
         |
         |   Reissue(base58'$assetId', $reissueAmount, true),
         |   Burn(base58'$assetId', $burnAmount),
         |   ScriptTransfer(Address(base58'$recipient'), $transferAmount, base58'$assetId'),
         |
         |   StringEntry("str", "str"),
         |   BinaryEntry("bin", base58'$assetId'),
         |
         |   Reissue(base58'$assetId', $reissueAmount, false),
         |   Burn(base58'$assetId', $burnAmount),
         |   ScriptTransfer(Address(base58'$recipient'), $transferAmount, base58'$assetId')
         | ]
       """.stripMargin
    )

  def checkStateAsset(
      startAmount: Long,
      reissueAmount: Long,
      burnAmount: Long,
      transferAmount: Long,
      recipient: Address
  ): Script = {
    val reissueCheckAmount1  = startAmount
    val burnCheckAmount1     = reissueCheckAmount1 + reissueAmount
    val transferCheckAmount1 = burnCheckAmount1 - burnAmount

    val reissueCheckAmount2  = transferCheckAmount1
    val burnCheckAmount2     = reissueCheckAmount2 + reissueAmount
    val transferCheckAmount2 = burnCheckAmount2 - burnAmount

    assetVerifier(
      s"""
         | let recipient = Address(base58'$recipient')
         |
         | func checkState(expectedAmount1: Int, expectedAmount2: Int) =
         |   this.issuer.getInteger("int") == 1     &&
         |   this.issuer.getBoolean("bool") == true &&
         |   (
         |     this.quantity == expectedAmount1      &&     # first action evaluation
         |     this.issuer.getString("str") == unit  &&
         |     this.issuer.getBinary("bin") == unit  &&
         |     recipient.assetBalance(this.id) == 0  ||
         |
         |     this.quantity == expectedAmount2        &&   # second action evaluation
         |     this.issuer.getString("str") == "str"   &&
         |     this.issuer.getBinary("bin") == this.id &&
         |     recipient.assetBalance(this.id) == $transferAmount
         |   )
         |
         | match tx {
         |   case t: ReissueTransaction =>
         |     t.quantity == $reissueAmount &&
         |     checkState($reissueCheckAmount1, $reissueCheckAmount2)
         |
         |   case t: BurnTransaction =>
         |     t.quantity == $burnAmount &&
         |     checkState($burnCheckAmount1, $burnCheckAmount2)
         |
         |   case t: TransferTransaction =>
         |     t.amount == $transferAmount &&
         |     checkState($transferCheckAmount1, $transferCheckAmount2)
         |
         |   case _ => throw("unexpected")
         | }
      """.stripMargin
    )
  }

  private def multiActionPreconditions(
      invokeFee: Long,
      withScriptError: Boolean
  ): (List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction, KeyPair, KeyPair, Long, Long, Long) = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val startAmount              = 100
    val reissueAmount            = 50
    val burnAmount               = 20
    val transferAmount           = 15
    val assetCheckTransferAmount = if (withScriptError) transferAmount + 1 else transferAmount

    val genesis = List(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val issue =
      TxHelpers.issue(
        master,
        startAmount,
        script = Some(checkStateAsset(startAmount, reissueAmount, burnAmount, assetCheckTransferAmount, invoker.toAddress)),
        fee = 1.004 waves
      )
    val setScript = TxHelpers.setScript(master, multiActionDApp(issue.id(), invoker.publicKey.toAddress, reissueAmount, burnAmount, transferAmount))
    val invoke    = TxHelpers.invoke(master.toAddress, func = None, invoker = invoker, fee = invokeFee)

    (genesis, setScript, invoke, issue, master, invoker, reissueAmount, burnAmount, transferAmount)
  }

  private def assetVerifier(body: String): Script =
    TestCompiler(V4).compileAsset(
      s"""
         | {-# STDLIB_VERSION 4          #-}
         | {-# CONTENT_TYPE   EXPRESSION #-}
         | {-# SCRIPT_TYPE    ASSET      #-}
         |
         | $body
         |
       """.stripMargin
    )

  private def reissueAndBurnDApp(assetId: ByteStr, reissueAmount: Long, burnAmount: Long, fail: Boolean): Script =
    dApp(
      s"""
         | [
         |   Reissue(base58'$assetId', $reissueAmount, true),
         |   Burn(base58'$assetId', $burnAmount)
         | ]
       """.stripMargin,
      fail
    )

  private def sponsorFeeDApp(minSponsoredAssetFee: Option[Long]): Script =
    dApp(
      s"""
         | let i0 = Issue("SponsoredAsset0", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
         | [
         |   i0,
         |   SponsorFee(calculateAssetId(i0), ${minSponsoredAssetFee.getOrElse(GlobalValNames.Unit)})
         | ]
       """.stripMargin
    )

  private def dApp(body: String, fail: Boolean = false): Script =
    TestCompiler(V4).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   strict c = ${if (fail) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"}
         |   $body
         | }
       """.stripMargin
    )

  private def issuePreconditions(
      invokeFee: Long
  ): (SetScriptTransaction, InvokeScriptTransaction, KeyPair, KeyPair, Long) = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val amount = 100

    val setScript = TxHelpers.setScript(master, issueDApp(amount))
    val invoke    = TxHelpers.invoke(master.toAddress, func = None, invoker = invoker, fee = invokeFee)

    (setScript, invoke, master, invoker, amount)
  }

  private def issueDApp(
      issueAmount: Long,
      name: String = "ScriptAsset",
      description: String = "Issued by InvokeScript",
      decimals: Int = 0,
      reissuable: Boolean = false
  ): Script =
    dApp(
      s"""
         | [
         |   Issue("$name", "$description", $issueAmount, $decimals, $reissuable, unit, 0)
         | ]
       """.stripMargin
    )

  property("issue action results state") {
    val (setScript, invoke, master, invoker, amount) = issuePreconditions(1.005.waves)
    withDomain(balances = AddrWithBalance.enoughBalances(invoker, master)) { d =>
      val tb1 = TestBlock.create(System.currentTimeMillis(), d.blockchain.lastBlockId.get, Seq(setScript)).block
      d.blockchainUpdater.processBlock(tb1, ByteStr(new Array[Byte](32)), None, verify = false).explicitGet()
      val tb2 = TestBlock.create(System.currentTimeMillis(), tb1.signature, Seq(invoke)).block
      d.blockchainUpdater.processBlock(tb2, ByteStr(new Array[Byte](32)), None, verify = false).explicitGet()

      d.portfolio(master.toAddress).map(_._2) shouldEqual Seq(amount)
      d.portfolio(invoker.toAddress) shouldEqual Seq()

      d.blockchainUpdater.processBlock(
        TestBlock.create(System.currentTimeMillis(), tb2.signature, Seq.empty).block,
        ByteStr(new Array[Byte](32)),
        None,
        verify = false
      )

      d.portfolio(master.toAddress).map(_._2) shouldEqual Seq(amount)
      d.portfolio(invoker.toAddress) shouldEqual Seq()
    }
  }

  property("sponsor fee action results state") {
    val (genesis, setScript, invoke, minSponsoredAssetFee) = sponsorFeePreconditions(1.005.waves)
    assertDiffAndState(
      Seq(TestBlock.create(genesis :+ setScript)),
      TestBlock.create(Seq(invoke)),
      features
    ) { case (diff, blockchain) =>
      val asset = diff.issuedAssets.head._1
      diff.sponsorship shouldBe Map(asset -> SponsorshipValue(minSponsoredAssetFee))
      blockchain.assetDescription(asset).map(_.sponsorship) shouldBe Some(minSponsoredAssetFee)
    }
  }
}
