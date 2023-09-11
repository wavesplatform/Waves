package com.wavesplatform.it.asset

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.BaseFreeSpec
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.{IssueInfoResponse, SponsorFeeResponse, StateChangesDetails}
import com.wavesplatform.it.sync.*
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.Sponsorship
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class SponsorFeeActionSuite extends BaseFreeSpec {
  private val initialWavesBalance = 100.waves

  private var sponsoredAssetId: String  = ""
  private var globalDAppAddress: String = ""
  private var dApp: KeyPair             = _
  private val minSponsoredAssetFee      = 100

  override def beforeAll(): Unit = {
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

    globalDAppAddress = dApp.toAddress.toString
  }

  "State changes" - {
    "Issue and sponsor from dApp" in {
      val invokeTx = miner.invokeScript(miner.keyPair, globalDAppAddress, Some("issueAndSponsor"), waitForTx = true, fee = smartMinFee + issueFee)

      val matchDebugResult = matchPattern {
        case Seq(
              StateChangesDetails(
                Nil,
                Nil,
                Seq(IssueInfoResponse(issueAssetId, _, _, _, _, _, _, _)),
                Nil,
                Nil,
                Seq(SponsorFeeResponse(sponsorFeeAssetId, Some(`minSponsoredAssetFee`))),
                None,
                Nil
              )
            ) if issueAssetId == sponsorFeeAssetId =>
      }

      val stateChanges = miner.stateChanges(invokeTx._1.id).stateChanges.toSeq
      stateChanges should matchDebugResult
      miner.transactionsByAddress(globalDAppAddress, limit = 100).flatMap(_.stateChanges) should matchDebugResult

      sponsoredAssetId = stateChanges.head.sponsorFees.head.assetId
      miner.assetsDetails(sponsoredAssetId).minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)

      val dAppBalance = miner.assetsBalance(globalDAppAddress).balances.head
      dAppBalance.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
      dAppBalance.sponsorBalance shouldBe Some(miner.balance(globalDAppAddress).balance)
    }

    "Use sponsored asset as fee" in {
      val firstCheckHeight = miner.height + 1
      nodes.waitForHeight(firstCheckHeight)

      val alice = miner.createKeyPair()
      val bob   = miner.createKeyPair()

      val startDAppSponsorAssetBalance = miner.assetBalance(globalDAppAddress, sponsoredAssetId).balance
      val startDAppBalance             = miner.balance(globalDAppAddress).balance
      val startMinerBalance            = miner.balanceAtHeight(miner.address, firstCheckHeight)

      val assetFee            = 100
      val assetTransferAmount = 1000

      val aliceAddress = alice.toAddress.toString
      miner.transfer(dApp, aliceAddress, assetFee + assetTransferAmount, assetId = Some(sponsoredAssetId), fee = smartMinFee, waitForTx = true)
      val bobAddress = bob.toAddress.toString
      miner.transfer(
        alice,
        bobAddress,
        assetTransferAmount,
        assetId = Some(sponsoredAssetId),
        fee = assetFee,
        feeAssetId = Some(sponsoredAssetId),
        waitForTx = true
      )

      miner.assetBalance(aliceAddress, sponsoredAssetId).balance shouldBe 0
      miner.assetBalance(bobAddress, sponsoredAssetId).balance shouldBe assetTransferAmount

      val dAppWavesOutgo = smartMinFee + Sponsorship.toWaves(assetFee, minSponsoredAssetFee)
      val blockReward    = miner.lastBlock().reward.get

      val lastCheckHeight = miner.height + 1
      miner.waitForHeight(lastCheckHeight)

      miner.assetBalance(globalDAppAddress, sponsoredAssetId).balance shouldBe startDAppSponsorAssetBalance - assetTransferAmount
      miner.balanceAtHeight(globalDAppAddress, lastCheckHeight) shouldBe startDAppBalance - dAppWavesOutgo

      miner.balanceAtHeight(miner.address, lastCheckHeight) shouldBe
        startMinerBalance + dAppWavesOutgo + blockReward * (lastCheckHeight - firstCheckHeight)
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

      val dAppAddress = dApp.toAddress.toString
      val issueTx     = miner.invokeScript(miner.keyPair, dAppAddress, Some("issue2assets"), waitForTx = true, fee = smartMinFee + issueFee * 2)
      miner.invokeScript(miner.keyPair, dAppAddress, Some("sponsor2assets"), waitForTx = true, fee = smartMinFee)
      val cancelTx = miner.invokeScript(miner.keyPair, dAppAddress, Some("cancelSponsorship"), waitForTx = true, fee = smartMinFee)

      val assetIdByName    = miner.stateChanges(issueTx._1.id).stateChanges.get.issues.map(issue => (issue.name, issue.assetId)).toMap
      val sponsoredAssetId = assetIdByName("SponsoredAsset1")
      val cancelledAssetId = assetIdByName("SponsoredAsset0")

      miner
        .stateChanges(cancelTx._1.id)
        .stateChanges
        .toSeq
        .find(_.sponsorFees == Seq(SponsorFeeResponse(`cancelledAssetId`, None)))
        .head

      miner
        .transactionsByAddress(dAppAddress, limit = 100)
        .flatMap(_.stateChanges)
        .find(_.sponsorFees == Seq(SponsorFeeResponse(`cancelledAssetId`, None)))
        .head

      miner.assetsDetails(sponsoredAssetId).minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)

      val dAppBalance = miner.assetsBalance(dAppAddress).balances.map(b => (b.assetId, b)).toMap
      dAppBalance(sponsoredAssetId).minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
      dAppBalance(sponsoredAssetId).sponsorBalance shouldBe Some(miner.balance(dAppAddress).balance)
      dAppBalance(cancelledAssetId).minSponsoredAssetFee shouldBe None
      dAppBalance(cancelledAssetId).sponsorBalance shouldBe None
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

      val dAppAddress = dApp.toAddress.toString
      val invokeTx = miner.invokeScript(miner.keyPair, dAppAddress, Some("issueAndMultipleSponsor"), waitForTx = true, fee = smartMinFee + issueFee)
      val txStateChanges = miner.stateChanges(invokeTx._1.id).stateChanges.toSeq
      val assetId        = txStateChanges.flatMap(_.issues).head.assetId

      val matchDebugResult = matchPattern {
        case Seq(
              StateChangesDetails(
                Nil,
                Nil,
                Seq(IssueInfoResponse(`assetId`, _, _, _, _, _, _, _)),
                Nil,
                Nil,
                sponsorFeeResponses,
                None,
                Nil
              )
            ) if sponsorFeeResponses.size == 9 && sponsorFeeResponses.last == SponsorFeeResponse(`assetId`, Some(`lastMinSponsoredAssetFee`)) =>
      }
      txStateChanges should matchDebugResult
      miner.transactionsByAddress(dAppAddress, limit = 100).flatMap(_.stateChanges) should matchDebugResult

      miner.assetsDetails(assetId).minSponsoredAssetFee shouldBe Some(lastMinSponsoredAssetFee)

      val dAppBalance = miner.assetsBalance(dAppAddress).balances.head
      dAppBalance.minSponsoredAssetFee shouldBe Some(lastMinSponsoredAssetFee)
      dAppBalance.sponsorBalance shouldBe Some(miner.balance(dAppAddress).balance)
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

      val dAppAddress    = dApp.toAddress.toString
      val invokeTx       = miner.invokeScript(miner.keyPair, dAppAddress, Some("sponsorAndCancel"), waitForTx = true, fee = smartMinFee + issueFee)
      val txStateChanges = miner.stateChanges(invokeTx._1.id).stateChanges.toSeq
      val assetId        = txStateChanges.flatMap(_.issues).head.assetId

      val matchDebugResult = matchPattern {
        case Seq(
              StateChangesDetails(
                Nil,
                Nil,
                Seq(IssueInfoResponse(`assetId`, _, _, _, _, _, _, _)),
                Nil,
                Nil,
                Seq(SponsorFeeResponse(`assetId`, Some(100)), SponsorFeeResponse(`assetId`, None)),
                None,
                Nil
              )
            ) =>
      }
      txStateChanges should matchDebugResult
      miner.transactionsByAddress(dAppAddress, limit = 100).flatMap(_.stateChanges) should matchDebugResult

      miner.assetsDetails(assetId).minSponsoredAssetFee shouldBe None

      val dAppBalance = miner.assetsBalance(dAppAddress).balances.head
      dAppBalance.minSponsoredAssetFee shouldBe None
      dAppBalance.sponsorBalance shouldBe None
    }
  }

  "Restrictions" - {
    "SponsorFee is available for assets issued via transaction" in {
      val dApp = miner.createKeyPair()
      miner.transfer(sender.keyPair, dApp.toAddress.toString, initialWavesBalance, minFee, waitForTx = true)
      val assetId = miner.issue(dApp, waitForTx = true).id

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

      val tx = miner.invokeScript(miner.keyPair, dApp.toAddress.toString, Some("sponsorAsset"), waitForTx = true, fee = smartMinFee)
      sender.stateChanges(tx._1.id).stateChanges.get.sponsorFees.head shouldBe SponsorFeeResponse(assetId, Some(1000))
    }

    "Negative fee is not available" in {
      val dApp        = miner.createKeyPair()
      val dAppAddress = dApp.toAddress.toString
      miner.transfer(sender.keyPair, dAppAddress, initialWavesBalance, minFee, waitForTx = true)
      val assetId = miner.issue(dApp, waitForTx = true).id

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

      assertBadRequestAndMessage(
        miner.invokeScript(miner.keyPair, dAppAddress, Some("sponsorAsset"), fee = smartMinFee),
        "Negative sponsor amount = -1"
      )
    }

    "SponsorFee is available only for assets issuing from current address" in {
      val issuer = miner.createKeyPair()
      miner.transfer(sender.keyPair, issuer.toAddress.toString, initialWavesBalance, minFee, waitForTx = true)
      val assetId = miner.issue(issuer, waitForTx = true).id

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

      assertBadRequestAndMessage(
        miner.invokeScript(miner.keyPair, dApp.toAddress.toString, Some("sponsorAsset"), waitForTx = true, fee = smartMinFee),
        s"SponsorFee assetId=$assetId was not issued from address of current dApp"
      )
    }

    "SponsorFee is not available for scripted assets" in {
      val dApp        = miner.createKeyPair()
      val dAppAddress = dApp.toAddress.toString
      miner.transfer(sender.keyPair, dAppAddress, initialWavesBalance, minFee, waitForTx = true)

      val script  = ScriptCompiler.compile("true", ScriptEstimatorV2).explicitGet()._1.bytes().base64
      val assetId = miner.issue(dApp, script = Some(script), waitForTx = true).id

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
      assertBadRequestAndMessage(
        miner.invokeScript(miner.keyPair, dAppAddress, Some("sponsorAsset"), fee = smartMinFee + smartFee),
        "Sponsorship smart assets is disabled."
      )
    }
  }

  "Rollback" - {
    val minSponsoredAssetFee = 1000
    val script =
      s"""
         | {-# STDLIB_VERSION 4 #-}
         | {-# CONTENT_TYPE DAPP #-}
         | {-# SCRIPT_TYPE ACCOUNT #-}
         |
         | @Callable(i)
         | func issueAsset() = {
         |     let issue = Issue("SponsoredAsset0", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
         |     let assetId = calculateAssetId(issue)
         |     [
         |         issue,
         |         BinaryEntry("sponsoredAssetId", assetId)
         |     ]
         | }
         |
         | @Callable(i)
         | func sponsorAsset() = [
         |     SponsorFee(this.getBinary("sponsoredAssetId").value(), $minSponsoredAssetFee)
         | ]
      """.stripMargin

    "without returning to utx" in {
      val dApp = createDApp(script).toAddress.toString

      val invokeTx1     = miner.invokeScript(miner.keyPair, dApp, Some("issueAsset"), waitForTx = true, fee = smartMinFee + issueFee)
      val assetId       = miner.stateChanges(invokeTx1._1.id).stateChanges.get.issues.head.assetId
      val firstTxHeight = miner.height
      nodes.waitForHeight(firstTxHeight + 1)

      val invokeTx2 = miner.invokeScript(miner.keyPair, dApp, Some("sponsorAsset"), waitForTx = true, fee = smartMinFee + issueFee)
      miner.stateChanges(invokeTx2._1.id).stateChanges.get.sponsorFees.head.assetId shouldBe assetId

      nodes.rollback(firstTxHeight, returnToUTX = false)
      nodes.waitForHeight(miner.height + 1)

      miner.assetsDetails(assetId).minSponsoredAssetFee shouldBe None
      val dAppBalance = miner.assetsBalance(dApp).balances.head
      dAppBalance.assetId shouldBe assetId
      dAppBalance.minSponsoredAssetFee shouldBe None
      dAppBalance.sponsorBalance shouldBe None
    }

    "with returning to utx" in {
      val dAppAddress   = createDApp(script).toAddress.toString
      val invokeTx1     = miner.invokeScript(miner.keyPair, dAppAddress, Some("issueAsset"), waitForTx = true, fee = smartMinFee + issueFee)
      val assetId       = miner.stateChanges(invokeTx1._1.id).stateChanges.get.issues.head.assetId
      val firstTxHeight = miner.height
      nodes.waitForHeight(firstTxHeight + 1)

      val invokeTx2 = miner.invokeScript(miner.keyPair, dAppAddress, Some("sponsorAsset"), waitForTx = true, fee = smartMinFee + issueFee)
      miner.stateChanges(invokeTx2._1.id).stateChanges.get.sponsorFees.head.assetId shouldBe assetId

      nodes.rollback(firstTxHeight)
      nodes.waitForTransaction(invokeTx2._1.id)

      miner.assetsDetails(assetId).minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
      val dAppBalance = miner.assetsBalance(dAppAddress).balances.head
      dAppBalance.assetId shouldBe assetId
      dAppBalance.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
      dAppBalance.sponsorBalance shouldBe Some(miner.balance(dAppAddress).balance)
    }
  }

  private def createDApp(script: String, address: KeyPair = miner.createKeyPair()): KeyPair = {
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    miner.transfer(sender.keyPair, address.toAddress.toString, initialWavesBalance, minFee, waitForTx = true)

    nodes.waitForTransaction(
      miner
        .signedBroadcast(
          SetScriptTransaction
            .selfSigned(1.toByte, address, Some(compiledScript), setScriptFee, System.currentTimeMillis())
            .explicitGet()
            .json()
        )
        .id
    )

    address
  }
}
