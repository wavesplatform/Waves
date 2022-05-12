package com.wavesplatform.it.asset

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.api.{IssueInfoResponse, SponsorFeeResponse, StateChangesDetails}
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.sync.grpc.GrpcBaseTransactionSuiteLike
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.Sponsorship
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.freespec.AnyFreeSpec

class GrpcSponsorFeeActionSuite extends AnyFreeSpec with GrpcBaseTransactionSuiteLike {
  private val initialWavesBalance = 100.waves

  private val minSponsoredAssetFee          = 100
  private var sponsoredAssetId: String      = ""
  private var globalDAppAddress: ByteString = ""
  private var dApp: KeyPair                 = _

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    dApp = createDApp(
      s"""
         |  {-# STDLIB_VERSION 4 #-}
         |  {-# CONTENT_TYPE DAPP #-}
         |  {-# SCRIPT_TYPE ACCOUNT #-}
         |
         |  @Callable(i)
         |  func issueAndSponsor() = {
         |      let issue = Issue("SponsoredAsset", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
         |      let issueId = calculateAssetId(issue)
         |      [issue, SponsorFee(issueId, $minSponsoredAssetFee)]
         |  }
      """.stripMargin
    )

    globalDAppAddress = byteStringAddress(dApp)
  }

  "State changes" - {
    "Issue and sponsor from dApp" in {
      val invokeTx = miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User("issueAndSponsor"), List.empty)),
        fee = smartMinFee + issueFee,
        waitForTx = true
      )

      val matchDebugResult = matchPattern {
        case StateChangesDetails(
              Nil,
              Nil,
              Seq(IssueInfoResponse(issueAssetId, _, _, _, _, _, _, _)),
              Nil,
              Nil,
              Seq(SponsorFeeResponse(sponsorFeeAssetId, Some(`minSponsoredAssetFee`))),
              None,
              Nil
            ) if issueAssetId == sponsorFeeAssetId =>
      }

      val (_, stateChanges) = miner.stateChanges(invokeTx.id)
      stateChanges should matchDebugResult
      val (_, addressStateChanges) = miner.stateChanges(minerAddress).head
      addressStateChanges should matchDebugResult

      sponsoredAssetId = stateChanges.sponsorFees.head.assetId
      miner.assetInfo(sponsoredAssetId).sponsorship shouldBe minSponsoredAssetFee

      val (assetId, _) = miner.assetsBalance(globalDAppAddress).filter(_._1.nonEmpty).head
      val assetInfo    = miner.assetInfo(assetId)
      assetInfo.sponsorship shouldBe minSponsoredAssetFee
      assetInfo.sponsorBalance shouldBe miner.wavesBalance(globalDAppAddress).regular
    }

    "Use sponsored asset as fee" in {
      nodes.foreach(_.waitForHeight(miner.height + 1))

      val alice = miner.generateKeyPair()
      val bob   = miner.generateKeyPair()

      val (_, startDAppSponsorAssetBalance) = miner.assetsBalance(globalDAppAddress, Seq(sponsoredAssetId)).head
      val startDAppBalance                  = miner.wavesBalance(globalDAppAddress).regular

      val assetFee            = 100
      val assetTransferAmount = 1000

      miner.broadcastTransfer(
        dApp,
        PBRecipients.create(alice.toAddress),
        assetFee + assetTransferAmount,
        assetId = sponsoredAssetId,
        fee = smartMinFee,
        waitForTx = true
      )
      miner.broadcastTransfer(
        alice,
        PBRecipients.create(bob.toAddress),
        assetTransferAmount,
        assetId = sponsoredAssetId,
        fee = assetFee,
        feeAssetId = sponsoredAssetId,
        waitForTx = true
      )

      miner.assetsBalance(byteStringAddress(alice), Seq(sponsoredAssetId)).head._2 shouldBe 0
      miner.assetsBalance(byteStringAddress(bob), Seq(sponsoredAssetId)).head._2 shouldBe assetTransferAmount

      val dAppWavesOutgo = smartMinFee + Sponsorship.toWaves(assetFee, minSponsoredAssetFee)

      miner.waitForHeight(miner.height + 1)

      miner.assetsBalance(globalDAppAddress, Seq(sponsoredAssetId)).head._2 shouldBe startDAppSponsorAssetBalance - assetTransferAmount
      miner.wavesBalance(globalDAppAddress).regular shouldBe startDAppBalance - dAppWavesOutgo
    }

    "Cancel sponsorship" in {
      val dApp = createDApp(
        s"""
           |
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func issue2assets() = {
           |    let i1 = Issue("SponsoredAsset0", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
           |    let i2 = Issue("SponsoredAsset1", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
           |
           |    let issueId0 = calculateAssetId(i1)
           |    let issueId1 = calculateAssetId(i2)
           |
           |    [
           |        BinaryEntry("cancelSponsorAssetId0", issueId0),
           |        BinaryEntry("cancelSponsorAssetId1", issueId1),
           |        i1, i2
           |    ]
           |}
           |
           |@Callable(i)
           |func sponsor2assets() = [
           |    SponsorFee(this.getBinary("cancelSponsorAssetId0").value(), $minSponsoredAssetFee),
           |    SponsorFee(this.getBinary("cancelSponsorAssetId1").value(), $minSponsoredAssetFee)
           |]
           |
           |@Callable(i)
           |func cancelSponsorship() = [
           |    SponsorFee(this.getBinary("cancelSponsorAssetId0").value(), unit)
           |]
        """.stripMargin
      )

      val dAppAddress = byteStringAddress(dApp)
      val issueTx = miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User("issue2assets"), List.empty)),
        waitForTx = true,
        fee = smartMinFee + issueFee * 2
      )
      miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User("sponsor2assets"), List.empty)),
        waitForTx = true,
        fee = smartMinFee
      )
      val cancelTx = miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User("cancelSponsorship"), List.empty)),
        waitForTx = true,
        fee = smartMinFee
      )

      val assetIdByName    = miner.stateChanges(issueTx.id)._2.issues.map(issue => (issue.name, issue.assetId)).toMap
      val sponsoredAssetId = assetIdByName("SponsoredAsset1")
      val cancelledAssetId = assetIdByName("SponsoredAsset0")

      miner.stateChanges(cancelTx.id)._2.sponsorFees shouldBe Seq(SponsorFeeResponse(cancelledAssetId, None))
      miner.stateChanges(minerAddress).map(_._2).find(_.sponsorFees == Seq(SponsorFeeResponse(cancelledAssetId, None))) shouldBe defined

      val dAppBalance = miner.assetsBalance(dAppAddress)

      dAppBalance.contains(sponsoredAssetId) shouldBe true
      dAppBalance.contains(cancelledAssetId) shouldBe true

      val sponsoredAsset = miner.assetInfo(sponsoredAssetId)
      sponsoredAsset.sponsorship shouldBe minSponsoredAssetFee
      sponsoredAsset.sponsorBalance shouldBe miner.wavesBalance(dAppAddress).regular

      val cancelledAsset = miner.assetInfo(cancelledAssetId)
      cancelledAsset.sponsorship shouldBe 0
      cancelledAsset.sponsorBalance shouldBe 0
    }

    "Multiple SponsorFee is available for same asset" in {
      val lastMinSponsoredAssetFee = 10000000000L
      val dApp = createDApp(
        s"""
           | {-# STDLIB_VERSION 4 #-}
           | {-# CONTENT_TYPE DAPP #-}
           | {-# SCRIPT_TYPE ACCOUNT #-}
           |
           | let issue = Issue("SponsoredAsset0", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
           | let assetId = calculateAssetId(issue)
           |
           | @Callable(i)
           | func issueAndMultipleSponsor() = [
           |     issue,
           |     SponsorFee(assetId, 100),
           |     SponsorFee(assetId, 1000),
           |     SponsorFee(assetId, 10000),
           |     SponsorFee(assetId, 100000),
           |     SponsorFee(assetId, 1000000),
           |     SponsorFee(assetId, 10000000),
           |     SponsorFee(assetId, 100000000),
           |     SponsorFee(assetId, 1000000000),
           |     SponsorFee(assetId, $lastMinSponsoredAssetFee)
           | ]
        """.stripMargin
      )

      val dAppAddress = byteStringAddress(dApp)
      val invokeTx = miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User("issueAndMultipleSponsor"), List.empty)),
        waitForTx = true,
        fee = smartMinFee + issueFee
      )

      val txStateChanges = miner.stateChanges(invokeTx.id)._2
      val assetId        = txStateChanges.issues.head.assetId

      val matchDebugResult = matchPattern {
        case StateChangesDetails(
              Nil,
              Nil,
              Seq(IssueInfoResponse(`assetId`, _, _, _, _, _, _, _)),
              Nil,
              Nil,
              sponsorFeeResponses,
              None,
              Nil
            ) if sponsorFeeResponses.size == 9 && sponsorFeeResponses.last == SponsorFeeResponse(`assetId`, Some(`lastMinSponsoredAssetFee`)) =>
      }
      txStateChanges should matchDebugResult
      miner.stateChanges(minerAddress).map(_._2).head should matchDebugResult

      miner.assetsBalance(dAppAddress).contains(assetId) shouldBe true

      val assetInfo = miner.assetInfo(assetId)
      assetInfo.sponsorship shouldBe lastMinSponsoredAssetFee
      assetInfo.sponsorBalance shouldBe miner.wavesBalance(dAppAddress).regular
    }

    "Sponsor and cancel sponsorship is available for same asset" in {
      val dApp = createDApp(
        s"""
           | {-# STDLIB_VERSION 4 #-}
           | {-# CONTENT_TYPE DAPP #-}
           | {-# SCRIPT_TYPE ACCOUNT #-}
           |
           | let issue = Issue("SponsoredAsset0", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
           | let assetId = calculateAssetId(issue)
           |
           | @Callable(i)
           | func sponsorAndCancel() = [
           |     issue,
           |     SponsorFee(assetId, 100),
           |     SponsorFee(assetId, unit)
           | ]
        """.stripMargin
      )

      val dAppAddress = byteStringAddress(dApp)
      val invokeTx =
        miner.broadcastInvokeScript(
          miner.keyPair,
          PBRecipients.create(dApp.toAddress),
          Some(FUNCTION_CALL(FunctionHeader.User("sponsorAndCancel"), List.empty)),
          waitForTx = true,
          fee = smartMinFee + issueFee
        )
      val txStateChanges = miner.stateChanges(invokeTx.id)._2
      val assetId        = txStateChanges.issues.head.assetId

      val matchDebugResult = matchPattern {
        case StateChangesDetails(
              Nil,
              Nil,
              Seq(IssueInfoResponse(`assetId`, _, _, _, _, _, _, _)),
              Nil,
              Nil,
              Seq(SponsorFeeResponse(`assetId`, Some(100)), SponsorFeeResponse(`assetId`, None)),
              None,
              Nil
            ) =>
      }
      txStateChanges should matchDebugResult
      miner.stateChanges(minerAddress).map(_._2).head should matchDebugResult

      miner.assetsBalance(dAppAddress).contains(assetId) shouldBe true

      val assetInfo = miner.assetInfo(assetId)
      assetInfo.sponsorship shouldBe 0
      assetInfo.sponsorBalance shouldBe 0
    }
  }

  "Restrictions" - {
    "SponsorFee is available for assets issued via transaction" in {
      val dApp = miner.generateKeyPair()
      miner.broadcastTransfer(sender.keyPair, PBRecipients.create(dApp.toAddress), initialWavesBalance, minFee, waitForTx = true)
      val assetId = miner.broadcastIssue(dApp, "test", 100, 8, reissuable = true, fee = issueFee, waitForTx = true).id

      createDApp(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func sponsorAsset() = [
           |    SponsorFee(base58'$assetId', 1000)
           |]
        """.stripMargin,
        dApp
      )

      val tx = miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User("sponsorAsset"), List.empty)),
        waitForTx = true,
        fee = smartMinFee
      )
      sender.stateChanges(tx.id)._2.sponsorFees.head shouldBe SponsorFeeResponse(assetId, Some(1000))
    }

    "Negative fee is not available" in {
      val dApp = miner.generateKeyPair()
      miner.broadcastTransfer(sender.keyPair, PBRecipients.create(dApp.toAddress), initialWavesBalance, minFee, waitForTx = true)
      val assetId = miner.broadcastIssue(dApp, "test", 100, 8, reissuable = true, fee = issueFee, waitForTx = true).id

      createDApp(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func sponsorAsset() = [
           |    SponsorFee(base58'$assetId', -1)
           |]
        """.stripMargin,
        dApp
      )

      assertGrpcError(
        miner.broadcastInvokeScript(
          miner.keyPair,
          PBRecipients.create(dApp.toAddress),
          Some(FUNCTION_CALL(FunctionHeader.User("sponsorAsset"), List.empty)),
          waitForTx = true,
          fee = smartMinFee
        ),
        "Negative sponsor amount = -1"
      )
    }

    "SponsorFee is available only for assets issuing from current address" in {
      val issuer = miner.generateKeyPair()
      miner.broadcastTransfer(sender.keyPair, PBRecipients.create(issuer.toAddress), initialWavesBalance, minFee, waitForTx = true)
      val assetId = miner.broadcastIssue(issuer, "test", 100, 8, reissuable = true, fee = issueFee, waitForTx = true).id

      val dApp = createDApp(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func sponsorAsset() = [
           |    SponsorFee(base58'$assetId', 1000)
           |]
        """.stripMargin
      )

      assertGrpcError(
        miner.broadcastInvokeScript(
          miner.keyPair,
          PBRecipients.create(dApp.toAddress),
          Some(FUNCTION_CALL(FunctionHeader.User("sponsorAsset"), List.empty)),
          waitForTx = true,
          fee = smartMinFee
        ),
        s"SponsorFee assetId=$assetId was not issued from address of current dApp"
      )
    }

    "SponsorFee is not available for scripted assets" in {
      val dApp = miner.generateKeyPair()
      miner.broadcastTransfer(sender.keyPair, PBRecipients.create(dApp.toAddress), initialWavesBalance, minFee, waitForTx = true)

      val script  = ScriptCompiler.compile("true", ScriptEstimatorV2).explicitGet()._1
      val assetId = miner.broadcastIssue(dApp, "test", 100, 8, reissuable = true, script = Right(Some(script)), fee = issueFee, waitForTx = true).id

      createDApp(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func sponsorAsset() = [
           |    SponsorFee(base58'$assetId', 1000)
           |]
        """.stripMargin,
        dApp
      )

      assertGrpcError(
        miner.broadcastInvokeScript(
          miner.keyPair,
          PBRecipients.create(dApp.toAddress),
          Some(FUNCTION_CALL(FunctionHeader.User("sponsorAsset"), List.empty)),
          fee = smartMinFee + smartFee
        ),
        "Sponsorship smart assets is disabled."
      )
    }
  }

  private def createDApp(script: String, address: KeyPair = miner.generateKeyPair()): KeyPair = {
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    miner.broadcastTransfer(
      sender.keyPair,
      PBRecipients.create(address.toAddress),
      initialWavesBalance,
      fee = minFee,
      waitForTx = true
    )

    nodes.foreach(
      _.waitForTxAndHeightArise(
        miner
          .setScript(address, Right(Some(compiledScript)), fee = smartMinFee + setScriptFee, timestamp = System.currentTimeMillis(), waitForTx = true)
          .id
      )
    )

    address
  }

  private def byteStringAddress(account: KeyPair): ByteString =
    PBRecipients.create(account.toAddress).getPublicKeyHash

  private def minerAddress = byteStringAddress(miner.keyPair)
}
