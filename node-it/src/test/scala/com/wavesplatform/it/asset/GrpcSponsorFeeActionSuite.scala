package com.wavesplatform.it.asset

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{IssueInfoResponse, SponsorFeeResponse, StateChangesDetails}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.grpc.GrpcBaseTransactionSuiteLike
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.Sponsorship
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.FreeSpec

class GrpcSponsorFeeActionSuite extends FreeSpec with GrpcBaseTransactionSuiteLike {
  private val initialWavesBalance = 100.waves

  private val minSponsoredAssetFee      = 100
  private var sponsoredAssetId: String  = ""
  private var globalDAppAddress: String = ""
  private var dApp: KeyPair             = _

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

      val stateChanges = miner.debugStateChanges(invokeTx._1.id).stateChanges.toSeq
      stateChanges should matchDebugResult
      miner.debugStateChangesByAddress(globalDAppAddress, limit = 100).flatMap(_.stateChanges) should matchDebugResult

      sponsoredAssetId = stateChanges.head.sponsorFees.head.assetId
      miner.assetsDetails(sponsoredAssetId).minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)

      val dAppBalance = miner.assetsBalance(globalDAppAddress).balances.head
      dAppBalance.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
      dAppBalance.sponsorBalance shouldBe Some(miner.balance(globalDAppAddress).balance)
    }

    "Use sponsored asset as fee" in {
      nodes.waitForHeight(miner.height + 1)

      val alice = miner.createKeyPair()
      val bob   = miner.createKeyPair()

      val dAppAddress                  = globalDAppAddress
      val startDAppSponsorAssetBalance = miner.assetBalance(dAppAddress, sponsoredAssetId).balance
      val startDAppBalance             = miner.balance(dAppAddress).balance
      val startMinerBalance            = miner.balance(miner.address).balance

      val assetFee            = 100
      val assetTransferAmount = 1000

      miner.transfer(
        dApp,
        alice.toAddress.toString,
        assetFee + assetTransferAmount,
        assetId = Some(sponsoredAssetId),
        fee = smartMinFee,
        waitForTx = true
      )
      miner.transfer(
        alice,
        bob.toAddress.toString,
        assetTransferAmount,
        assetId = Some(sponsoredAssetId),
        fee = assetFee,
        feeAssetId = Some(sponsoredAssetId),
        waitForTx = true
      )

      miner.assetBalance(alice.toAddress.toString, sponsoredAssetId).balance shouldBe 0
      miner.assetBalance(bob.toAddress.toString, sponsoredAssetId).balance shouldBe assetTransferAmount

      val dAppWavesOutgo = smartMinFee + Sponsorship.toWaves(assetFee, minSponsoredAssetFee)
      val blockReward    = miner.lastBlock().reward.get

      miner.waitForHeight(miner.height + 1)

      miner.assetBalance(dAppAddress, sponsoredAssetId).balance shouldBe startDAppSponsorAssetBalance - assetTransferAmount
      miner.balance(dAppAddress).balance shouldBe startDAppBalance - dAppWavesOutgo
      miner.balance(miner.address).balance shouldBe startMinerBalance + dAppWavesOutgo + blockReward
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

      val assetIdByName    = miner.debugStateChanges(issueTx._1.id).stateChanges.get.issues.map(issue => (issue.name, issue.assetId)).toMap
      val sponsoredAssetId = assetIdByName("SponsoredAsset1")
      val cancelledAssetId = assetIdByName("SponsoredAsset0")

      miner
        .debugStateChanges(cancelTx._1.id)
        .stateChanges
        .toSeq
        .find(_.sponsorFees == Seq(SponsorFeeResponse(`cancelledAssetId`, None)))
        .head

      miner
        .debugStateChangesByAddress(dAppAddress, limit = 100)
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

      val dAppAddress    = dApp.toAddress.toString
      val invokeTx       = miner.invokeScript(miner.keyPair, dAppAddress, Some("issueAndMultipleSponsor"), waitForTx = true, fee = smartMinFee + issueFee)
      val txStateChanges = miner.debugStateChanges(invokeTx._1.id).stateChanges.toSeq
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
      miner.debugStateChangesByAddress(dAppAddress, limit = 100).flatMap(_.stateChanges) should matchDebugResult

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

      val invokeTx =
        miner.invokeScript(miner.keyPair, dApp.toAddress.toString, Some("sponsorAndCancel"), waitForTx = true, fee = smartMinFee + issueFee)
      val txStateChanges = miner.debugStateChanges(invokeTx._1.id).stateChanges.toSeq
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
      miner.debugStateChangesByAddress(dApp.toAddress.toString, limit = 100).flatMap(_.stateChanges) should matchDebugResult

      miner.assetsDetails(assetId).minSponsoredAssetFee shouldBe None

      val dAppBalance = miner.assetsBalance(dApp.toAddress.toString).balances.head
      dAppBalance.minSponsoredAssetFee shouldBe None
      dAppBalance.sponsorBalance shouldBe None
    }
  }

  "Restrictions" - {
    "SponsorFee actions count limit" in {
      val minSponsoredAssetFee = 1001

      val dApp = createDApp(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func issue10assets() = {
           |    let i0 = Issue("SponsoredAsset0", "SponsoredAsset description", 1000000000000000, 2, true, unit, 0)
           |    let i1 = Issue("SponsoredAsset1", "SponsoredAsset description", 1000000000000000, 2, true, unit, 1)
           |    let i2 = Issue("SponsoredAsset2", "SponsoredAsset description", 1000000000000000, 2, true, unit, 2)
           |    let i3 = Issue("SponsoredAsset3", "SponsoredAsset description", 1000000000000000, 2, true, unit, 3)
           |    let i4 = Issue("SponsoredAsset4", "SponsoredAsset description", 1000000000000000, 2, true, unit, 4)
           |    let i5 = Issue("SponsoredAsset5", "SponsoredAsset description", 1000000000000000, 2, true, unit, 5)
           |    let i6 = Issue("SponsoredAsset6", "SponsoredAsset description", 1000000000000000, 2, true, unit, 6)
           |    let i7 = Issue("SponsoredAsset7", "SponsoredAsset description", 1000000000000000, 2, true, unit, 7)
           |    let i8 = Issue("SponsoredAsset8", "SponsoredAsset description", 1000000000000000, 2, true, unit, 8)
           |    let i9 = Issue("SponsoredAsset9", "SponsoredAsset description", 1000000000000000, 2, true, unit, 9)
           |
           |    let issueId0 = calculateAssetId(i0)
           |    let issueId1 = calculateAssetId(i1)
           |    let issueId2 = calculateAssetId(i2)
           |    let issueId3 = calculateAssetId(i3)
           |    let issueId4 = calculateAssetId(i4)
           |    let issueId5 = calculateAssetId(i5)
           |    let issueId6 = calculateAssetId(i6)
           |    let issueId7 = calculateAssetId(i7)
           |    let issueId8 = calculateAssetId(i8)
           |    let issueId9 = calculateAssetId(i9)
           |
           |    [
           |        BinaryEntry("sponsoredAssetId0", issueId0),
           |        BinaryEntry("sponsoredAssetId1", issueId1),
           |        BinaryEntry("sponsoredAssetId2", issueId2),
           |        BinaryEntry("sponsoredAssetId3", issueId3),
           |        BinaryEntry("sponsoredAssetId4", issueId4),
           |        BinaryEntry("sponsoredAssetId5", issueId5),
           |        BinaryEntry("sponsoredAssetId6", issueId6),
           |        BinaryEntry("sponsoredAssetId7", issueId7),
           |        BinaryEntry("sponsoredAssetId8", issueId8),
           |        BinaryEntry("sponsoredAssetId9", issueId9),
           |        i0,i1,i2,i3,i4,i5,i6,i7,i8,i9
           |    ]
           |}
           |
           |@Callable(i)
           |func sponsor10assets() = [
           |    SponsorFee(this.getBinary("sponsoredAssetId0").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId1").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId2").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId3").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId4").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId5").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId6").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId7").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId8").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId9").value(), ${minSponsoredAssetFee.toString})
           |]
           |
           |@Callable(i)
           |func sponsor11assets() = [
           |    SponsorFee(this.getBinary("sponsoredAssetId0").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId1").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId2").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId3").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId4").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId5").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId6").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId7").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId8").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId9").value(), ${minSponsoredAssetFee.toString}),
           |    SponsorFee(this.getBinary("sponsoredAssetId9").value(), ${minSponsoredAssetFee.toString})
           |]
        """.stripMargin
      )
      val dappAddress = dApp.toAddress.toString
      val invokeTx1   = miner.invokeScript(miner.keyPair, dappAddress, Some("issue10assets"), waitForTx = true, fee = smartMinFee + issueFee * 10)
      val invokeTx2   = miner.invokeScript(miner.keyPair, dappAddress, Some("sponsor10assets"), waitForTx = true, fee = smartMinFee)

      nodes.waitForHeightAriseAndTxPresent(invokeTx1._1.id)
      nodes.waitForHeightAriseAndTxPresent(invokeTx2._1.id)

      val assetIds    = miner.debugStateChanges(invokeTx1._1.id).stateChanges.get.issues.map(_.assetId)
      val sponsorFees = miner.debugStateChanges(invokeTx2._1.id).stateChanges.get.sponsorFees

      (assetIds zip sponsorFees)
        .foreach {
          case (issueAssetId, sponsorFee) =>
            issueAssetId shouldBe sponsorFee.assetId
            sponsorFee.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)

            miner.assetsDetails(issueAssetId).minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
            val dAppBalance = miner.assetsBalance(dappAddress).balances.find(_.assetId == issueAssetId).get
            dAppBalance.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
            dAppBalance.sponsorBalance shouldBe Some(miner.balance(dappAddress).balance)
        }

      assertBadRequestAndMessage(
        miner.invokeScript(miner.keyPair, dappAddress, Some("sponsor11assets"), fee = smartMinFee),
        "Actions count limit is exceeded"
      )
    }

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
      sender.debugStateChanges(tx._1.id).stateChanges.get.sponsorFees.head shouldBe SponsorFeeResponse(assetId, Some(1000))
    }

    "Negative fee is not available" in {
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
           |    SponsorFee(base58'$assetId', -1)
           |]
        """.stripMargin,
        dApp
      )

      assertBadRequestAndMessage(
        miner.invokeScript(miner.keyPair, dApp.toAddress.toString, Some("sponsorAsset"), waitForTx = true, fee = smartMinFee),
        "NegativeMinFee"
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
      ).toAddress.toString

      assertBadRequestAndMessage(
        miner.invokeScript(miner.keyPair, dApp, Some("sponsorAsset"), waitForTx = true, fee = smartMinFee),
        s"SponsorFee assetId=$assetId was not issued from address of current dApp"
      )
    }

    "SponsorFee is not available for scripted assets" in {
      val dApp = miner.createKeyPair()
      miner.transfer(sender.keyPair, dApp.toAddress.toString, initialWavesBalance, minFee, waitForTx = true)

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
        miner.invokeScript(miner.keyPair, dApp.toAddress.toString, Some("sponsorAsset"), fee = smartMinFee + smartFee),
        "Sponsorship smart assets is disabled."
      )
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

    miner.transfer(sender.keyPair, address.publicKey.toAddress.toString, initialWavesBalance, minFee, waitForTx = true)

    nodes.waitForHeightAriseAndTxPresent(
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
