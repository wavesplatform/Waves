package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, NodeConfigs, ReportingTestName}
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Assertion, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class SponsorshipSuite
    extends FreeSpec
    with IntegrationSuiteWithThreeAddresses
    with NodesFromDocker
    with Matchers
    with ReportingTestName
    /*with CancelAfterFailure*/ {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((14, 1000000)))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.blocks-for-feature-activation=1"))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.feature-check-blocks-period=1"))
      .withDefault(1)
      .withSpecial(1, _.nonMiner)
      .buildNonConflicting()

  val sponsor           = firstAddress
  val alice             = secondAddress
  val bob               = thirdAddress
  val Waves             = 100000000L
  val Token             = 100L
  val sponsorAssetTotal = 100 * Token
  val minSponsorFee     = Token
  val TinyFee           = Token / 2
  val SmallFee          = Token + Token / 2
  val LargeFee          = 10 * Token
  var sponsorWavesBalance = 0L
  var minerWavesBalance = 0L
  var minerWavesBalanceAfterFirstXferTest = 0L
  var sponsorWavesBalanceAfterFirstXferTest = 0L
  var firstSponsorAssetId: String = ""
  var secondSponsorAssetId: String = ""
  var firstTransferTxToAlice: String = ""
  var secondTransferTxToAlice: String = ""
  var firstSponsorTxId: String = ""
  var secondSponsorTxId: String = ""

  def assertMinAssetFee(txId: String, sponsorship: Long): Assertion = {
    val txInfo = miner.transactionInfo[TransactionInfo](txId)
    assert(txInfo.minSponsoredAssetFee.contains(sponsorship))
  }

  def assertSponsorship(assetId: String, sponsorship: Long): Assertion = {
    val assetInfo = miner.assetsDetails(assetId)
    assert(assetInfo.minSponsoredAssetFee == Some(sponsorship).filter(_ != 0))
  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sponsorWavesBalance = sender.accountBalances(sponsor)._2
    minerWavesBalance = sender.accountBalances(miner.address)._2
    minerWavesBalanceAfterFirstXferTest   = minerWavesBalance + 2 * issueFee + 2 * sponsorFee + 2 * minFee + 2 * FeeValidation.FeeUnit * SmallFee / minSponsorFee
    sponsorWavesBalanceAfterFirstXferTest = sponsorWavesBalance - 2 * issueFee - 2 * sponsorFee - 2 * minFee - 2 * FeeValidation.FeeUnit * SmallFee / minSponsorFee

    firstSponsorAssetId =
      sender
        .issue(
          sponsor,
          "AssetTxV1",
          "Created by Sponsorship Suite",
          sponsorAssetTotal,
          decimals = 2,
          reissuable = false,
          fee = issueFee,
          waitForTx = true
        )
        .id
    secondSponsorAssetId =
      sender
        .issue(
          sponsor,
          "AssetTxV2",
          "Created by Sponsorship Suite",
          sponsorAssetTotal,
          decimals = 2,
          reissuable = false,
          fee = issueFee,
          waitForTx = true
        )
        .id

    firstTransferTxToAlice = sender.transfer(sponsor, alice, sponsorAssetTotal / 2, minFee, Some(firstSponsorAssetId), None, waitForTx = true).id
    secondTransferTxToAlice = sender.transfer(sponsor, alice, sponsorAssetTotal / 2, minFee, Some(secondSponsorAssetId), None, waitForTx = true).id
    firstSponsorTxId  = sender.sponsorAsset(sponsor, firstSponsorAssetId, baseFee = Token, fee = sponsorFee, version = TxVersion.V1).id
    secondSponsorTxId = sender.sponsorAsset(sponsor, secondSponsorAssetId, baseFee = Token, fee = sponsorFee, version = TxVersion.V2).id
  }

  "Fee in sponsored asset works fine for transaction" - {

    "make assets sponsored" in {
      nodes.waitForHeightAriseAndTxPresent(firstSponsorTxId)
      nodes.waitForHeightAriseAndTxPresent(secondSponsorTxId)
      sender.transactionInfo[TransactionInfo](secondSponsorTxId).chainId shouldBe Some(AddressScheme.current.chainId)

      assertSponsorship(firstSponsorAssetId, 1 * Token)
      assertSponsorship(secondSponsorAssetId, 1 * Token)
      assertMinAssetFee(firstSponsorTxId, 1 * Token)
      assertMinAssetFee(secondSponsorTxId, 1 * Token)
    }

    "check balance before test accounts balances" in {
      for (sponsorAssetId <- Seq(firstSponsorAssetId, secondSponsorAssetId)) {
        sender.assertAssetBalance(sponsor, sponsorAssetId, sponsorAssetTotal / 2)
        sender.assertAssetBalance(alice, sponsorAssetId, sponsorAssetTotal / 2)

        val assetInfo = sender.assetsBalance(alice).balances.filter(_.assetId == sponsorAssetId).head
        assetInfo.minSponsoredAssetFee shouldBe Some(Token)
        assetInfo.sponsorBalance shouldBe Some(sender.accountBalances(sponsor)._2)
      }
    }

    "sender cannot make transfer" - {
      "invalid tx timestamp" in {
        for (v <- sponsorshipTxSupportedVersions) {
          def invalidTx(timestamp: Long): SponsorFeeTransaction =
            SponsorFeeTransaction
              .selfSigned(
                version = v,
                pkByAddress(sponsor),
                IssuedAsset(ByteStr.decodeBase58(firstSponsorAssetId).get),
                Some(SmallFee),
                minFee,
                timestamp + 1.day.toMillis
              )
              .right
              .get

          val iTx = invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis)
          assertBadRequestAndResponse(sender.broadcastRequest(iTx.json()), "Transaction timestamp .* is more than .*ms in the future")
        }
      }
    }

    "fee should be written off in issued asset" - {

      "alice transfer sponsored asset to bob using sponsored fee" in {
        val firstTransferTxCustomFeeAlice = sender.transfer(alice, bob, 10 * Token, SmallFee, Some(firstSponsorAssetId), Some(firstSponsorAssetId)).id
        val secondTransferTxCustomFeeAlice =
          sender.transfer(alice, bob, 10 * Token, SmallFee, Some(secondSponsorAssetId), Some(secondSponsorAssetId)).id
        nodes.waitForHeightArise()
        nodes.waitForTransaction(firstTransferTxCustomFeeAlice)
        nodes.waitForTransaction(secondTransferTxCustomFeeAlice)

        sender.assertAssetBalance(alice, firstSponsorAssetId, sponsorAssetTotal / 2 - SmallFee - 10 * Token)
        sender.assertAssetBalance(alice, secondSponsorAssetId, sponsorAssetTotal / 2 - SmallFee - 10 * Token)
        sender.assertAssetBalance(bob, firstSponsorAssetId, 10 * Token)
        sender.assertAssetBalance(bob, secondSponsorAssetId, 10 * Token)

        val aliceTxs = sender.transactionsByAddress(alice, 100)
        aliceTxs.size shouldBe 5 //not 4, because there was one more transaction in IntegrationSuiteWithThreeAddresses class
        aliceTxs.count(tx => tx.sender.contains(alice) || tx.recipient.contains(alice)) shouldBe 5
        aliceTxs.map(_.id) should contain allElementsOf Seq(
          firstTransferTxToAlice,
          secondTransferTxToAlice,
          firstTransferTxCustomFeeAlice,
          secondTransferTxCustomFeeAlice
        )

        val bobTxs = sender.transactionsByAddress(bob, 100)
        bobTxs.size shouldBe 3
        bobTxs.count(tx => tx.sender.contains(bob) || tx.recipient.contains(bob)) shouldBe 3
        bobTxs.map(_.id) should contain allElementsOf Seq(firstTransferTxCustomFeeAlice, secondTransferTxCustomFeeAlice)
      }

      "check transactions by address" in {
        val minerTxs = sender.transactionsByAddress(miner.address, 100)
        minerTxs.size shouldBe 4

        val sponsorTxs = sender.transactionsByAddress(sponsor, 100)
        sponsorTxs.size shouldBe 9 //TODO: bug?
        sponsorTxs.count(tx => tx.sender.contains(sponsor) || tx.recipient.contains(sponsor)) shouldBe 7
        sponsorTxs.map(_.id) should contain allElementsOf Seq(
          firstSponsorAssetId,
          secondSponsorAssetId,
          firstTransferTxToAlice,
          secondTransferTxToAlice,
          firstSponsorTxId,
          secondSponsorTxId
        )
      }

      "sponsor should receive sponsored asset as fee, waves should be written off" in {
        miner.assertAssetBalance(sponsor, firstSponsorAssetId, sponsorAssetTotal / 2 + SmallFee)
        miner.assertAssetBalance(sponsor, secondSponsorAssetId, sponsorAssetTotal / 2 + SmallFee)
        miner.assertBalances(sponsor, sponsorWavesBalanceAfterFirstXferTest)
      }

      "miner waves balance should be changed" in {
        miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest)
      }
    }

    "assets balance should contain sponsor fee info and sponsor balance" in {
      val sponsorLeaseSomeWaves = sender.lease(sponsor, bob, leasingAmount, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(sponsorLeaseSomeWaves)
      val (_, sponsorEffectiveBalance)   = sender.accountBalances(sponsor)
      val aliceFirstSponsorAssetBalance  = sender.assetsBalance(alice).balances.filter(_.assetId == firstSponsorAssetId).head
      val aliceSecondSponsorAssetBalance = sender.assetsBalance(alice).balances.filter(_.assetId == secondSponsorAssetId).head
      aliceFirstSponsorAssetBalance.minSponsoredAssetFee shouldBe Some(minSponsorFee)
      aliceSecondSponsorAssetBalance.minSponsoredAssetFee shouldBe Some(minSponsorFee)
      aliceFirstSponsorAssetBalance.sponsorBalance shouldBe Some(sponsorEffectiveBalance)
      aliceSecondSponsorAssetBalance.sponsorBalance shouldBe Some(sponsorEffectiveBalance)
    }

    "waves fee depends on sponsor fee and sponsored token decimals" in {
      val transferTxCustomLargeFeeAlice1 = sender.transfer(alice, bob, 1.waves, LargeFee, None, Some(firstSponsorAssetId)).id
      val transferTxCustomLargeFeeAlice2 = sender.transfer(alice, bob, 1.waves, LargeFee, None, Some(secondSponsorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(transferTxCustomLargeFeeAlice1)
      nodes.waitForHeightAriseAndTxPresent(transferTxCustomLargeFeeAlice2)

      sender.assertAssetBalance(sponsor, firstSponsorAssetId, sponsorAssetTotal / 2 + SmallFee + LargeFee)
      sender.assertAssetBalance(sponsor, secondSponsorAssetId, sponsorAssetTotal / 2 + SmallFee + LargeFee)
      sender.assertAssetBalance(alice, firstSponsorAssetId, sponsorAssetTotal / 2 - SmallFee - LargeFee - 10 * Token)
      sender.assertAssetBalance(alice, secondSponsorAssetId, sponsorAssetTotal / 2 - SmallFee - LargeFee - 10 * Token)
      sender.assertAssetBalance(bob, firstSponsorAssetId, 10 * Token)
      sender.assertAssetBalance(bob, secondSponsorAssetId, 10 * Token)
      sender.assertBalances(
        sponsor,
        sponsorWavesBalanceAfterFirstXferTest - FeeValidation.FeeUnit * 2 * LargeFee / Token - leasingFee,
        sponsorWavesBalanceAfterFirstXferTest - FeeValidation.FeeUnit * 2 * LargeFee / Token - leasingFee - leasingAmount
      )
      miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest + FeeValidation.FeeUnit * 2 * LargeFee / Token + leasingFee)
    }

    "cancel sponsorship" - {

      "cancel" in {
        val cancelFirstSponsorTxId  = sender.cancelSponsorship(sponsor, firstSponsorAssetId, fee = issueFee, version = TxVersion.V1).id
        val cancelSecondSponsorTxId = sender.cancelSponsorship(sponsor, secondSponsorAssetId, fee = issueFee, version = TxVersion.V2).id
        nodes.waitForHeightAriseAndTxPresent(cancelFirstSponsorTxId)
        nodes.waitForHeightAriseAndTxPresent(cancelSecondSponsorTxId)
      }

      "check asset details info" in {
        for (sponsorAssetId <- Seq(firstSponsorAssetId, secondSponsorAssetId)) {
          val assetInfo = sender.assetsBalance(alice).balances.filter(_.assetId == sponsorAssetId).head
          assetInfo.minSponsoredAssetFee shouldBe None
          assetInfo.sponsorBalance shouldBe None
        }
      }

      "cannot pay fees in non sponsored assets" in {
        assertBadRequestAndResponse(
          sender.transfer(alice, bob, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(firstSponsorAssetId)).id,
          s"Asset $firstSponsorAssetId is not sponsored, cannot be used to pay fees"
        )
        assertBadRequestAndResponse(
          sender.transfer(alice, bob, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(secondSponsorAssetId)).id,
          s"Asset $secondSponsorAssetId is not sponsored, cannot be used to pay fees"
        )
      }

      "check cancel transaction info" in {
        assertSponsorship(firstSponsorAssetId, 0L)
        assertSponsorship(secondSponsorAssetId, 0L)
      }

      "check sponsor and miner balances after cancel" in {
        sender.assertBalances(
          sponsor,
          sponsorWavesBalanceAfterFirstXferTest - FeeValidation.FeeUnit * 2 * LargeFee / Token - leasingFee - 2 * issueFee,
          sponsorWavesBalanceAfterFirstXferTest - FeeValidation.FeeUnit * 2 * LargeFee / Token - leasingFee - leasingAmount - 2 * issueFee
        )
        miner.assertBalances(
          miner.address,
          minerWavesBalanceAfterFirstXferTest + FeeValidation.FeeUnit * 2 * LargeFee / Token + leasingFee + 2 * issueFee
        )
      }

      "cancel sponsorship again" in {
        val cancelSponsorshipTxId1 = sender.cancelSponsorship(sponsor, firstSponsorAssetId, fee = issueFee, version = TxVersion.V1).id
        val cancelSponsorshipTxId2 = sender.cancelSponsorship(sponsor, firstSponsorAssetId, fee = issueFee, version = TxVersion.V2).id
        nodes.waitForHeightArise()
        nodes.waitForTransaction(cancelSponsorshipTxId1)
        nodes.waitForTransaction(cancelSponsorshipTxId2)
      }

    }
    "set sponsopship again" - {

      "set sponsorship and check new asset details, min sponsored fee changed" in {
        val setAssetSponsoredTx1 = sender.sponsorAsset(sponsor, firstSponsorAssetId, fee = issueFee, baseFee = TinyFee, version = TxVersion.V1).id
        val setAssetSponsoredTx2 = sender.sponsorAsset(sponsor, secondSponsorAssetId, fee = issueFee, baseFee = TinyFee, version = TxVersion.V2).id
        nodes.waitForHeightAriseAndTxPresent(setAssetSponsoredTx1)
        nodes.waitForHeightAriseAndTxPresent(setAssetSponsoredTx2)
        for (sponsorAssetId <- Seq(firstSponsorAssetId, secondSponsorAssetId)) {
          val assetInfo = sender.assetsBalance(alice).balances.filter(_.assetId == sponsorAssetId).head
          assetInfo.minSponsoredAssetFee shouldBe Some(Token / 2)
          assetInfo.sponsorBalance shouldBe Some(sender.accountBalances(sponsor)._2)
        }
      }

      "make transfer with new min sponsored fee" in {
        val sponsoredBalance          = sender.accountBalances(sponsor)
        val sponsorFirstAssetBalance  = sender.assetBalance(sponsor, firstSponsorAssetId).balance
        val sponsorSecondAssetBalance = sender.assetBalance(sponsor, secondSponsorAssetId).balance
        val aliceFirstAssetBalance    = sender.assetBalance(alice, firstSponsorAssetId).balance
        val aliceSecondAssetBalance   = sender.assetBalance(alice, secondSponsorAssetId).balance
        val aliceWavesBalance         = sender.accountBalances(alice)
        val bobFirstAssetBalance      = sender.assetBalance(bob, firstSponsorAssetId).balance
        val bobSecondAssetBalance     = sender.assetBalance(bob, secondSponsorAssetId).balance
        val bobWavesBalance           = sender.accountBalances(bob)
        val minerBalance              = miner.accountBalances(miner.address)
        val minerFirstAssetBalance    = miner.assetBalance(miner.address, firstSponsorAssetId).balance
        val minerSecondAssetBalance   = miner.assetBalance(miner.address, secondSponsorAssetId).balance

        val transferTxCustomFeeAlice1 = sender.transfer(alice, bob, 1.waves, TinyFee, None, Some(firstSponsorAssetId)).id
        val transferTxCustomFeeAlice2 = sender.transfer(alice, bob, 1.waves, TinyFee, None, Some(secondSponsorAssetId)).id
        nodes.waitForHeightArise()
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice1)
        nodes.waitForTransaction(transferTxCustomFeeAlice2)

        val wavesFee = FeeValidation.FeeUnit * 2 * TinyFee / TinyFee
        sender.assertBalances(sponsor, sponsoredBalance._1 - wavesFee, sponsoredBalance._2 - wavesFee)
        sender.assertAssetBalance(sponsor, firstSponsorAssetId, sponsorFirstAssetBalance + TinyFee)
        sender.assertAssetBalance(sponsor, secondSponsorAssetId, sponsorSecondAssetBalance + TinyFee)
        sender.assertAssetBalance(alice, firstSponsorAssetId, aliceFirstAssetBalance - TinyFee)
        sender.assertAssetBalance(alice, secondSponsorAssetId, aliceSecondAssetBalance - TinyFee)
        sender.assertBalances(alice, aliceWavesBalance._1 - 2.waves, aliceWavesBalance._2 - 2.waves)
        sender.assertBalances(bob, bobWavesBalance._1 + 2.waves, bobWavesBalance._2 + 2.waves)
        sender.assertAssetBalance(bob, firstSponsorAssetId, bobFirstAssetBalance)
        sender.assertAssetBalance(bob, secondSponsorAssetId, bobSecondAssetBalance)
        miner.assertBalances(miner.address, minerBalance._2 + wavesFee)
        miner.assertAssetBalance(miner.address, firstSponsorAssetId, minerFirstAssetBalance)
        miner.assertAssetBalance(miner.address, secondSponsorAssetId, minerSecondAssetBalance)
      }

      "change sponsorship fee in active sponsored asset" in {
        val setAssetSponsoredTx1 = sender.sponsorAsset(sponsor, firstSponsorAssetId, fee = issueFee, baseFee = LargeFee, version = TxVersion.V1).id
        val setAssetSponsoredTx2 = sender.sponsorAsset(sponsor, secondSponsorAssetId, fee = issueFee, baseFee = LargeFee, version = TxVersion.V2).id
        nodes.waitForHeightArise()
        nodes.waitForHeightAriseAndTxPresent(setAssetSponsoredTx1)
        nodes.waitForTransaction(setAssetSponsoredTx2)

        for (sponsorAssetId <- Seq(firstSponsorAssetId, secondSponsorAssetId)) {
          val assetInfo = sender.assetsBalance(alice).balances.filter(_.assetId == sponsorAssetId).head
          assetInfo.minSponsoredAssetFee shouldBe Some(LargeFee)
          assetInfo.sponsorBalance shouldBe Some(sender.accountBalances(sponsor)._2)
        }
      }

      "transfer tx sponsored fee is less then new minimal" in {
        assertBadRequestAndResponse(
          sender
            .transfer(sponsor, alice, 11 * Token, fee = SmallFee, assetId = Some(firstSponsorAssetId), feeAssetId = Some(firstSponsorAssetId))
            .id,
          s"Fee for TransferTransaction \\($SmallFee in ${Some(firstSponsorAssetId).get}\\) does not exceed minimal value of 100000 WAVES or $LargeFee ${Some(firstSponsorAssetId).get}"
        )
        assertBadRequestAndResponse(
          sender
            .transfer(sponsor, alice, 11 * Token, fee = SmallFee, assetId = Some(secondSponsorAssetId), feeAssetId = Some(secondSponsorAssetId))
            .id,
          s"Fee for TransferTransaction \\($SmallFee in ${Some(secondSponsorAssetId).get}\\) does not exceed minimal value of 100000 WAVES or $LargeFee ${Some(secondSponsorAssetId).get}"
        )
      }

      "make transfer with updated min sponsored fee" in {
        val sponsoredBalance          = sender.accountBalances(sponsor)
        val sponsorFirstAssetBalance  = sender.assetBalance(sponsor, firstSponsorAssetId).balance
        val sponsorSecondAssetBalance = sender.assetBalance(sponsor, secondSponsorAssetId).balance
        val aliceFirstAssetBalance    = sender.assetBalance(alice, firstSponsorAssetId).balance
        val aliceSecondAssetBalance   = sender.assetBalance(alice, firstSponsorAssetId).balance
        val aliceWavesBalance         = sender.accountBalances(alice)
        val bobWavesBalance           = sender.accountBalances(bob)
        val minerBalance              = miner.accountBalances(miner.address)

        val transferTxCustomFeeAlice1 = sender.transfer(alice, bob, 1.waves, LargeFee, None, Some(firstSponsorAssetId)).id
        val transferTxCustomFeeAlice2 = sender.transfer(alice, bob, 1.waves, LargeFee, None, Some(secondSponsorAssetId)).id
        nodes.waitForHeightArise()
        nodes.waitForTransaction(transferTxCustomFeeAlice1)
        nodes.waitForTransaction(transferTxCustomFeeAlice2)
        val wavesFee = FeeValidation.FeeUnit * 2 * LargeFee / LargeFee
        nodes.waitForHeightArise()

        sender.assertBalances(sponsor, sponsoredBalance._1 - wavesFee, sponsoredBalance._2 - wavesFee)
        sender.assertAssetBalance(sponsor, firstSponsorAssetId, sponsorFirstAssetBalance + LargeFee)
        sender.assertAssetBalance(sponsor, secondSponsorAssetId, sponsorSecondAssetBalance + LargeFee)
        sender.assertAssetBalance(alice, firstSponsorAssetId, aliceFirstAssetBalance - LargeFee)
        sender.assertAssetBalance(alice, secondSponsorAssetId, aliceSecondAssetBalance - LargeFee)

        sender.assertBalances(alice, aliceWavesBalance._1 - 2.waves, aliceWavesBalance._2 - 2.waves)
        sender.assertBalances(bob, bobWavesBalance._1 + 2.waves, bobWavesBalance._2 + 2.waves)
        miner.assertBalances(miner.address, minerBalance._1 + wavesFee, minerBalance._2 + wavesFee)
      }

    }

    "issue asset make sponsor and burn and reissue" in {
      val sponsorBalance = sender.accountBalances(sponsor)
      val minerBalance   = miner.accountBalances(miner.address)

      val firstSponsorAssetId2 =
        sender
          .issue(
            sponsor,
            "Another1",
            "Created by Sponsorship Suite",
            sponsorAssetTotal,
            decimals = 2,
            reissuable = true,
            fee = issueFee,
            waitForTx = true
          )
          .id
      val secondSponsorAssetId2 =
        sender
          .issue(
            sponsor,
            "Another2",
            "Created by Sponsorship Suite",
            sponsorAssetTotal,
            decimals = 2,
            reissuable = true,
            fee = issueFee,
            waitForTx = true
          )
          .id
      val sponsorTxId1 = sender.sponsorAsset(sponsor, firstSponsorAssetId2, baseFee = Token, fee = sponsorFee, version = TxVersion.V1).id
      val sponsorTxId2 = sender.sponsorAsset(sponsor, secondSponsorAssetId2, baseFee = Token, fee = sponsorFee, version = TxVersion.V2).id
      sender.transfer(sponsor, alice, sponsorAssetTotal / 2, minFee, Some(firstSponsorAssetId2), None, waitForTx = true).id
      sender.transfer(sponsor, alice, sponsorAssetTotal / 2, minFee, Some(secondSponsorAssetId2), None, waitForTx = true).id
      nodes.waitForHeightAriseAndTxPresent(sponsorTxId1)
      nodes.waitForTransaction(sponsorTxId2)

      sender.burn(sponsor, firstSponsorAssetId2, sponsorAssetTotal / 2, burnFee, waitForTx = true).id
      sender.burn(sponsor, secondSponsorAssetId2, sponsorAssetTotal / 2, burnFee, waitForTx = true).id

      for (sponsorAssetId2 <- Seq(firstSponsorAssetId2, secondSponsorAssetId2)) {
        val assetInfo = sender.assetsDetails(sponsorAssetId2)
        assetInfo.minSponsoredAssetFee shouldBe Some(Token)
        assetInfo.quantity shouldBe sponsorAssetTotal / 2
      }

      sender.reissue(sponsor, firstSponsorAssetId2, sponsorAssetTotal, reissuable = true, issueFee, waitForTx = true).id
      sender.reissue(sponsor, secondSponsorAssetId2, sponsorAssetTotal, reissuable = true, issueFee, waitForTx = true).id

      for (sponsorAssetId2 <- Seq(firstSponsorAssetId2, secondSponsorAssetId2)) {
        val assetInfoAfterReissue = sender.assetsDetails(sponsorAssetId2)
        assetInfoAfterReissue.minSponsoredAssetFee shouldBe Some(Token)
        assetInfoAfterReissue.quantity shouldBe sponsorAssetTotal / 2 + sponsorAssetTotal
        assetInfoAfterReissue.reissuable shouldBe true
      }

      val aliceTransferWaves1 = sender.transfer(alice, bob, transferAmount, SmallFee, None, Some(firstSponsorAssetId2), waitForTx = true).id
      val aliceTransferWaves2 = sender.transfer(alice, bob, transferAmount, SmallFee, None, Some(secondSponsorAssetId2), waitForTx = true).id
      nodes.waitForHeightAriseAndTxPresent(aliceTransferWaves1)
      nodes.waitForHeightAriseAndTxPresent(aliceTransferWaves2)

      val totalWavesFee = FeeValidation.FeeUnit * 2 * SmallFee / Token + 2 * issueFee + 2 * sponsorFee + 2 * burnFee + 2 * minFee + 2 * issueFee
      miner.assertBalances(miner.address, minerBalance._1 + totalWavesFee, minerBalance._2 + totalWavesFee)
      sender.assertBalances(sponsor, sponsorBalance._1 - totalWavesFee, sponsorBalance._2 - totalWavesFee)
      sender.assertAssetBalance(sponsor, firstSponsorAssetId2, SmallFee + sponsorAssetTotal)
      sender.assertAssetBalance(sponsor, secondSponsorAssetId2, SmallFee + sponsorAssetTotal)
    }

    "miner is sponsor" in {
      val minerBalance = miner.accountBalances(miner.address)
      val firstMinersAsset =
        miner
        .issue(
          miner.address,
          "MinersAsset1",
          "Created by Sponsorship Suite",
          sponsorAssetTotal,
          decimals = 8,
          reissuable = true,
          fee = issueFee,
          waitForTx = true
        )
        .id
      val secondMinersAsset =
        miner
        .issue(
          miner.address,
          "MinersAsset2",
          "Created by Sponsorship Suite",
          sponsorAssetTotal,
          decimals = 8,
          reissuable = true,
          fee = issueFee,
          waitForTx = true
        )
        .id
      val firstSponsorshipTxId = miner.sponsorAsset(miner.address, firstMinersAsset, baseFee = Token, fee = sponsorFee, version = TxVersion.V1).id
      val secondSponsorshipTxId = miner.sponsorAsset(miner.address, secondMinersAsset, baseFee = Token, fee = sponsorFee, version = TxVersion.V2).id
      nodes.waitForHeightAriseAndTxPresent(firstSponsorshipTxId)
      nodes.waitForTransaction(secondSponsorshipTxId)
      val minerFirstTransferTxId =
        miner.transfer(miner.address, alice, sponsorAssetTotal / 2, SmallFee, Some(firstMinersAsset), Some(firstMinersAsset)).id
      val minerSecondTransferTxId =
        miner.transfer(miner.address, alice, sponsorAssetTotal / 2, SmallFee, Some(secondMinersAsset), Some(secondMinersAsset)).id
      nodes.waitForHeightAriseAndTxPresent(minerFirstTransferTxId)
      nodes.waitForHeightAriseAndTxPresent(minerSecondTransferTxId)

      miner.assertBalances(miner.address, minerBalance._1)
      val aliceFirstTransferWavesId = sender.transfer(alice, bob, transferAmount, SmallFee, None, Some(firstMinersAsset)).id
      val aliceSecondTransferWavesId = sender.transfer(alice, bob, transferAmount, SmallFee, None, Some(secondMinersAsset)).id
      nodes.waitForHeightAriseAndTxPresent(aliceFirstTransferWavesId)
      nodes.waitForHeightAriseAndTxPresent(aliceSecondTransferWavesId)

      miner.assertBalances(miner.address, minerBalance._1)
      miner.assertAssetBalance(miner.address, firstMinersAsset, sponsorAssetTotal / 2 + SmallFee)
      miner.assertAssetBalance(miner.address, secondMinersAsset, sponsorAssetTotal / 2 + SmallFee)
    }

    "tx is declined if sponsor has not enough effective balance to pay fee" in {
      val sponsorEffectiveBalance = sender.accountBalances(sponsor)._2
      sender.lease(sponsor, bob, sponsorEffectiveBalance - leasingFee, leasingFee, waitForTx = true).id
      assertBadRequestAndMessage(
        sender.transfer(alice, bob, 10 * Token, LargeFee, Some(firstSponsorAssetId), Some(firstSponsorAssetId)),
        "unavailable funds"
      )
      assertBadRequestAndMessage(
        sender.transfer(alice, bob, 10 * Token, LargeFee, Some(secondSponsorAssetId), Some(secondSponsorAssetId)),
        "unavailable funds"
      )
    }
  }

}
