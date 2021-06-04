package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state.diffs.{ENOUGH_AMT, _}
import com.wavesplatform.state.{EmptyDataEntry, SponsorshipValue}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxVersion}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class CallableV4DiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDomain with EitherValues {
  property("reissue and burn actions result state") {
    forAll(paymentPreconditions(feeMultiplier = 0)) {
      case (genesis, setScript, invoke, issue, master, reissueAmount, burnAmount) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        ) {
          case (_, blockchain) =>
            val asset        = IssuedAsset(issue.id())
            val resultAmount = issue.quantity + reissueAmount - burnAmount

            blockchain.assetDescription(asset).get.totalVolume shouldBe resultAmount
            blockchain.balance(master.toAddress, asset) shouldBe resultAmount
        }
    }
  }

  property("asset script can disallow reissue") {
    val disallowReissueAsset: Script =
      assetVerifier(
        """
          | match tx {
          |   case _: ReissueTransaction => false
          |   case _ => true
          | }
        """.stripMargin
      )

    forAll(paymentPreconditions(Some(disallowReissueAsset), feeMultiplier = 2)) {
      case (genesis, setScript, invoke, issue, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        )(_ should produce("Transaction is not allowed by script of the asset", requireFailed = true))
    }
  }

  property("asset script can disallow burn") {
    val disallowBurnAsset: Script =
      assetVerifier(
        """
          | match tx {
          |   case _: BurnTransaction => false
          |   case _ => true
          | }
        """.stripMargin
      )

    forAll(paymentPreconditions(Some(disallowBurnAsset), feeMultiplier = 2)) {
      case (genesis, setScript, invoke, issue, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        )(_ should produce("Transaction is not allowed by script of the asset", requireFailed = true))
    }
  }

  property("asset script can allow burn and reissue") {
    val allowBurnAndReissueAsset: Script =
      assetVerifier(
        """
          | match tx {
          |   case _: ReissueTransaction | BurnTransaction => true
          |   case _ => false
          | }
        """.stripMargin
      )

    forAll(paymentPreconditions(Some(allowBurnAndReissueAsset), feeMultiplier = 2)) {
      case (genesis, setScript, invoke, issue, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        )(_.explicitGet())
    }
  }

  property("action state changes affects subsequent actions") {
    forAll(multiActionPreconditions(feeMultiplier = 6, withScriptError = false)) {
      case (genesis, setScript, invoke, issue, master, invoker, reissueAmount, burnAmount, transferAmount) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        ) {
          case (_, blockchain) =>
            val asset                 = IssuedAsset(issue.id())
            val totalResultAmount     = issue.quantity + (reissueAmount - burnAmount) * 2
            val issuerResultAmount    = issue.quantity + (reissueAmount - burnAmount - transferAmount) * 2
            val recipientResultAmount = transferAmount * 2

            blockchain.assetDescription(asset).get.totalVolume shouldBe totalResultAmount
            blockchain.balance(master.toAddress, asset) shouldBe issuerResultAmount
            blockchain.balance(invoker.toAddress, asset) shouldBe recipientResultAmount
        }
    }
  }

  property("check fee") {
    val minimalFee = 6 * ScriptExtraFee + FeeConstants(InvokeScriptTransaction.typeId) * FeeValidation.FeeUnit
    forAll(multiActionPreconditions(feeMultiplier = 5, withScriptError = false)) {
      case (genesis, setScript, invoke, issue, _, _, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        )(_ should produce(s" with 6 total scripts invoked does not exceed minimal value of $minimalFee WAVES"))
    }
  }

  ignore("trace") {
    forAll(multiActionPreconditions(feeMultiplier = 6, withScriptError = true)) {
      case (genesis, setScript, invoke, issue, _, _, _, _, _) =>
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

    val deleteEntryPreconditions: Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, KeyPair)] =
      for {
        master  <- accountGen
        invoker <- accountGen
        ts      <- timestampGen
        fee     <- ciFee()
      } yield {
        val dApp = Some(deleteEntryDApp)
        for {
          genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
          genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
          setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, dApp, fee, ts + 2)
          ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, Nil, fee, Waves, ts + 3)
        } yield (List(genesis, genesis2), setDApp, ci, master)
      }.explicitGet()

    forAll(deleteEntryPreconditions) {
      case (genesis, setScript, invoke, master) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis :+ setScript)),
          TestBlock.create(Seq(invoke)),
          features
        ) {
          case (diff, _) =>
            diff.accountData(master.toAddress).data shouldBe
              Map(
                "key1" -> EmptyDataEntry("key1"),
                "key2" -> EmptyDataEntry("key2")
              )
        }
    }
  }

  private def paymentPreconditions(
      assetScript: Option[Script] = None,
      feeMultiplier: Int
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction, KeyPair, Long, Long)] =
    for {
      master        <- accountGen
      invoker       <- accountGen
      ts            <- timestampGen
      fee           <- ciFee(feeMultiplier)
      issue         <- issueV2TransactionGen(master, Gen.const(assetScript), reissuableParam = Some(true))
      reissueAmount <- positiveLongGen
      burnAmount    <- Gen.choose(0, reissueAmount)
    } yield {
      val dApp = Some(reissueAndBurnDApp(issue.id(), reissueAmount, burnAmount))
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, dApp, fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue, master, reissueAmount, burnAmount)
    }.explicitGet()

  private def sponsorFeePreconditions: Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, Option[Long])] =
    for {
      master               <- accountGen
      invoker              <- accountGen
      ts                   <- timestampGen
      fee                  <- ciFee(1).map(_ + FeeUnit * FeeConstants(IssueTransaction.typeId))
      minSponsoredAssetFee <- Gen.oneOf(None, Some(1000L))
    } yield {
      val dApp = Some(sponsorFeeDApp(minSponsoredAssetFee))
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, dApp, fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, minSponsoredAssetFee)
    }.explicitGet()

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
      feeMultiplier: Int,
      withScriptError: Boolean
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction, KeyPair, KeyPair, Long, Long, Long)] =
    for {
      master         <- accountGen
      invoker        <- accountGen
      ts             <- timestampGen
      fee            <- ciFee(feeMultiplier)
      startAmount    <- positiveLongGen
      reissueAmount  <- positiveLongGen
      burnAmount     <- Gen.choose(0, reissueAmount)
      transferAmount <- Gen.choose(0, reissueAmount - burnAmount)
      assetCheckTransferAmount = if (withScriptError) transferAmount + 1 else transferAmount
      assetScript              = Some(checkStateAsset(startAmount, reissueAmount, burnAmount, assetCheckTransferAmount, invoker.toAddress))
      issue <- issueV2TransactionGen(master, Gen.const(assetScript), reissuableParam = Some(true), quantityParam = Some(startAmount))
    } yield {
      val dApp = Some(multiActionDApp(issue.id(), invoker.publicKey.toAddress, reissueAmount, burnAmount, transferAmount))
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(TxVersion.V1, master, dApp, fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(TxVersion.V1, invoker, master.toAddress, None, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue, master, invoker, reissueAmount, burnAmount, transferAmount)
    }.explicitGet()

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

  private def reissueAndBurnDApp(assetId: ByteStr, reissueAmount: Long, burnAmount: Long): Script =
    dApp(
      s"""
         | [
         |   Reissue(base58'$assetId', $reissueAmount, true),
         |   Burn(base58'$assetId', $burnAmount)
         | ]
       """.stripMargin
    )

  private def sponsorFeeDApp(minSponsoredAssetFee: Option[Long]): Script =
    dApp(
      s"""
         | let i0 = Issue("SponsoredAsset0", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
         | [
         |   i0,
         |   SponsorFee(calculateAssetId(i0), ${minSponsoredAssetFee.getOrElse("unit")})
         | ]
       """.stripMargin
    )

  private def dApp(body: String): Script =
    TestCompiler(V4).compileContract(s"""
         | {-# STDLIB_VERSION 4       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() = {
         |   $body
         | }
       """.stripMargin)

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.BlockV5
    ).map(_.id -> 0).toMap
  )

  private def issuePreconditions(
      assetScript: Option[Script] = None,
      feeMultiplier: Int,
      issueFeeMultiplier: Int
  ): Gen[(List[Transaction], InvokeScriptTransaction, KeyPair, KeyPair, Long)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      fee     <- ciFee(feeMultiplier, issueFeeMultiplier)
      amount  <- Gen.choose(1L, 100000000L)
    } yield {
      val dApp = Some(issueDApp(amount))
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, dApp, fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2, setDApp), ci, master, invoker, amount)
    }.explicitGet()

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
    forAll(issuePreconditions(feeMultiplier = 7, issueFeeMultiplier = 1)) {
      case (genesis, invoke, master, invoker, amount) =>
        withDomain() { d =>
          val tb1 = TestBlock.create(genesis)
          d.blockchainUpdater.processBlock(tb1, ByteStr(new Array[Byte](32)), false).explicitGet()
          val tb2 = TestBlock.create(System.currentTimeMillis(), tb1.signature, Seq(invoke))
          d.blockchainUpdater.processBlock(tb2, ByteStr(new Array[Byte](32)), false).explicitGet()

          d.portfolio(master.toAddress).map(_._2) shouldEqual Seq(amount)
          d.portfolio(invoker.toAddress) shouldEqual Seq()

          d.blockchainUpdater.processBlock(
            TestBlock.create(System.currentTimeMillis(), tb2.signature, Seq.empty),
            ByteStr(new Array[Byte](32)),
            verify = false
          )

          d.portfolio(master.toAddress).map(_._2) shouldEqual Seq(amount)
          d.portfolio(invoker.toAddress) shouldEqual Seq()
        }
    }
  }

  property("sponsor fee action results state") {
    forAll(sponsorFeePreconditions) {
      case (genesis, setScript, invoke, minSponsoredAssetFee) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis :+ setScript)),
          TestBlock.create(Seq(invoke)),
          features
        ) {
          case (diff, blockchain) =>
            val asset            = diff.issuedAssets.head._1
            val sponsorshipValue = minSponsoredAssetFee.getOrElse(0L)
            diff.sponsorship shouldBe Map(asset -> SponsorshipValue(sponsorshipValue))
            blockchain.assetDescription(asset).map(_.sponsorship) shouldBe Some(sponsorshipValue)
        }
    }
  }
}
