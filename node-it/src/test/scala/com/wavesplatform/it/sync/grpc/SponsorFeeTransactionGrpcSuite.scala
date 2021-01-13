package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.state.diffs.FeeValidation
import io.grpc.Status.Code

class SponsorFeeTransactionGrpcSuite extends GrpcBaseTransactionSuite {
  val (alice, aliceAddress)     = (firstAcc, firstAddress)
  val (bob, bobAddress)         = (secondAcc, secondAddress)
  val (sponsor, sponsorAddress) = (thirdAcc, thirdAddress)
  val token             = 100L
  val sponsorAssetTotal = 100 * token
  val minSponsorFee     = token
  val tinyFee           = token / 2
  val smallFee          = token + token / 2
  val largeFee          = 10 * token
  val sponsorFeeTxSupportedVersions: List[Byte] = List(1, 2)

  test("able to make transfer with sponsored fee") {
    for (v <- sponsorFeeTxSupportedVersions) {
      val minerWavesBalance = miner.wavesBalance(ByteString.copyFrom(Base58.decode(miner.address)))
      val minerBalanceHeight = miner.height

      val sponsoredAssetId = PBTransactions.vanilla(
        miner.broadcastIssue(sponsor, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString

      val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
      miner.broadcastSponsorFee(sponsor, sponsoredAssetMinFee, fee = sponsorReducedFee, version = v, waitForTx = true)

      miner.broadcastTransfer(sponsor, Recipient().withPublicKeyHash(aliceAddress), sponsorAssetTotal / 2, minFee, assetId = sponsoredAssetId, waitForTx = true)

      val aliceWavesBalance = miner.wavesBalance(aliceAddress)
      val bobWavesBalance = miner.wavesBalance(bobAddress)
      val sponsorWavesBalance = miner.wavesBalance(sponsorAddress)
      val aliceAssetBalance = miner.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
      val bobAssetBalance = miner.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
      val sponsorAssetBalance = miner.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)

      miner.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, smallFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true)

      nodes.foreach(n => n.waitForHeight(n.height + 1))
      miner.wavesBalance(aliceAddress).available shouldBe aliceWavesBalance.available
      miner.wavesBalance(bobAddress).available shouldBe bobWavesBalance.available
      miner.wavesBalance(sponsorAddress).available shouldBe sponsorWavesBalance.available - FeeValidation.FeeUnit * smallFee / minSponsorFee
      miner.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe aliceAssetBalance - 10 * token - smallFee
      miner.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe bobAssetBalance + 10 * token
      miner.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe sponsorAssetBalance + smallFee

      val reward = (miner.height - minerBalanceHeight) * 600000000L
      miner.wavesBalance(ByteString.copyFrom(Base58.decode(miner.address))).available shouldBe
        minerWavesBalance.available + reward + sponsorReducedFee + issueFee + minFee + FeeValidation.FeeUnit * smallFee / minSponsorFee
    }
  }

  test("only issuer is able to sponsor asset") {
    for (v <- sponsorFeeTxSupportedVersions) {
      val sponsoredAssetId = PBTransactions.vanilla(
        miner.broadcastIssue(sponsor, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString

      val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
      assertGrpcError(
        miner.broadcastSponsorFee(alice, sponsoredAssetMinFee, fee = sponsorReducedFee, version = v),
        "Asset was issued by other address",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("sponsor is able to cancel sponsorship") {
    for (v <- sponsorFeeTxSupportedVersions) {
      val sponsoredAssetId = PBTransactions.vanilla(
        miner.broadcastIssue(alice, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString

      val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
      miner.broadcastSponsorFee(alice, sponsoredAssetMinFee, fee = sponsorReducedFee, version = v, waitForTx = true)

      /**
        * Cancel sponsorship by sponsor None amount of sponsored asset.
        * As it is optional to pass all parameters to PB objects (Amount(assetId: ByteString, amount: Long) in this case),
        * we can simply pass unspecific (None) amount by creating Amount(assetId: ByteString). SponsorFeeTransaction with
        * that kind of Amount will cancel sponsorship.
        **/
      val sponsoredAssetNullMinFee = Some(Amount(ByteString.copyFrom(Base58.decode(sponsoredAssetId))))
      miner.broadcastSponsorFee(alice, sponsoredAssetNullMinFee, fee = sponsorReducedFee, version = v, waitForTx = true)

      assertGrpcError(
        miner.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, smallFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true),
        s"Asset $sponsoredAssetId is not sponsored, cannot be used to pay fees",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("sponsor is able to update amount of sponsored fee") {
    for (v <- sponsorFeeTxSupportedVersions) {
      val sponsoredAssetId = PBTransactions.vanilla(
        miner.broadcastIssue(sponsor, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString

      miner.broadcastTransfer(sponsor, Recipient().withPublicKeyHash(aliceAddress), sponsorAssetTotal / 2, minFee, assetId = sponsoredAssetId, waitForTx = true)

      val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
      miner.broadcastSponsorFee(sponsor, sponsoredAssetMinFee, fee = sponsorReducedFee, version = v, waitForTx = true)

      val sponsoredAssetUpdatedMinFee = Some(Amount(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), largeFee))
      miner.broadcastSponsorFee(sponsor, sponsoredAssetUpdatedMinFee, fee = sponsorReducedFee, version = v, waitForTx = true)

      assertGrpcError(
        miner.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, smallFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true),
        s"does not exceed minimal value of $minFee WAVES or $largeFee $sponsoredAssetId",
        Code.INVALID_ARGUMENT
      )
      val aliceWavesBalance = miner.wavesBalance(aliceAddress)
      val bobWavesBalance = miner.wavesBalance(bobAddress)
      val sponsorWavesBalance = miner.wavesBalance(sponsorAddress)
      val aliceAssetBalance = miner.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
      val bobAssetBalance = miner.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
      val sponsorAssetBalance = miner.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)

      miner.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, largeFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true)

      miner.wavesBalance(aliceAddress).available shouldBe aliceWavesBalance.available
      miner.wavesBalance(bobAddress).available shouldBe bobWavesBalance.available
      miner.wavesBalance(sponsorAddress).available shouldBe sponsorWavesBalance.available - FeeValidation.FeeUnit * largeFee / largeFee
      miner.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe aliceAssetBalance - 10 * token - largeFee
      miner.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe bobAssetBalance + 10 * token
      miner.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe sponsorAssetBalance + largeFee
    }
  }
}
