package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.Recipient
import com.wavesplatform.state.diffs.FeeValidation
import io.grpc.Status.Code


class SponsorFeeTransactionGrpcSuite extends GrpcBaseTransactionSuite {
  val (alice, aliceAddress) = (firstAcc, firstAddress)
  val (bob, bobAddress) = (secondAcc, secondAddress)
  val (sponsor, sponsorAddress) = (thirdAcc, thirdAddress)
  val token             = 100L
  val sponsorAssetTotal = 100 * token
  val minSponsorFee     = token
  val tinyFee           = token / 2
  val smallFee          = token + token / 2
  val largeFee          = 10 * token

  test("able to make transfer with sponsored fee") {
    val minerWavesBalance = sender.grpc.wavesBalance(ByteString.copyFrom(Base58.decode(miner.address)))
    val minerBalanceHeight = sender.grpc.height

    val sponsoredAssetId = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(sponsor, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, sponsorFee, waitForTx = true)
    ).explicitGet().id().toString

    val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
    sender.grpc.broadcastSponsorFee(sponsor, sponsoredAssetMinFee, fee = sponsorFee, waitForTx = true)

    sender.grpc.broadcastTransfer(sponsor, Recipient().withPublicKeyHash(aliceAddress), sponsorAssetTotal / 2, minFee, assetId = sponsoredAssetId, waitForTx = true)

    val aliceWavesBalance = sender.grpc.wavesBalance(aliceAddress)
    val bobWavesBalance = sender.grpc.wavesBalance(bobAddress)
    val sponsorWavesBalance = sender.grpc.wavesBalance(sponsorAddress)
    val aliceAssetBalance = sender.grpc.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
    val bobAssetBalance = sender.grpc.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
    val sponsorAssetBalance = sender.grpc.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)

    sender.grpc.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, smallFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true)

    nodes.foreach(n => n.waitForHeight(n.height + 1))
    sender.grpc.wavesBalance(aliceAddress).available shouldBe aliceWavesBalance.available
    sender.grpc.wavesBalance(bobAddress).available shouldBe bobWavesBalance.available
    sender.grpc.wavesBalance(sponsorAddress).available shouldBe sponsorWavesBalance.available - FeeValidation.FeeUnit * smallFee / minSponsorFee
    sender.grpc.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe aliceAssetBalance - 10 * token - smallFee
    sender.grpc.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe bobAssetBalance + 10 * token
    sender.grpc.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe sponsorAssetBalance + smallFee

    val reward = (sender.grpc.height - minerBalanceHeight) * 600000000L
    sender.grpc.wavesBalance(ByteString.copyFrom(Base58.decode(miner.address))).available shouldBe
      minerWavesBalance.available + reward + sponsorFee + issueFee + minFee + FeeValidation.FeeUnit * smallFee / minSponsorFee
  }

  test("only issuer is able to sponsor asset") {
    val sponsoredAssetId = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(sponsor, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, sponsorFee, waitForTx = true)
    ).explicitGet().id().toString

    val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
    assertGrpcError(
      sender.grpc.broadcastSponsorFee(alice, sponsoredAssetMinFee, fee = sponsorFee),
    "Asset was issued by other address",
      Code.INVALID_ARGUMENT
    )
  }

  test("sponsor is able to cancel sponsorship") {
    val sponsoredAssetId = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(alice, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, sponsorFee, waitForTx = true)
    ).explicitGet().id().toString

    val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
    sender.grpc.broadcastSponsorFee(alice, sponsoredAssetMinFee, fee = sponsorFee, waitForTx = true)

    /**
      * Cancel sponsorship by sponsor None amount of sponsored asset.
      * As it is optional to pass all parameters to PB objects (Amount(assetId: ByteString, amount: Long) in this case),
      * we can simply pass unspecific (None) amount by creating Amount(assetId: ByteString). SponsorFeeTransaction with
      * that kind of Amount will cancel sponsorship.
    **/
    val sponsoredAssetNullMinFee = Some(Amount(ByteString.copyFrom(Base58.decode(sponsoredAssetId))))
    sender.grpc.broadcastSponsorFee(alice, sponsoredAssetNullMinFee, fee = sponsorFee, waitForTx = true)

    assertGrpcError(
    sender.grpc.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, smallFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true),
      s"Asset $sponsoredAssetId is not sponsored, cannot be used to pay fees",
    Code.INVALID_ARGUMENT
    )
  }

  test("sponsor is able to update amount of sponsored fee") {
    val sponsoredAssetId = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(sponsor, "SponsoredAsset", sponsorAssetTotal, 2, reissuable = false, sponsorFee, waitForTx = true)
    ).explicitGet().id().toString

    sender.grpc.broadcastTransfer(sponsor, Recipient().withPublicKeyHash(aliceAddress), sponsorAssetTotal / 2, minFee, assetId = sponsoredAssetId, waitForTx = true)

    val sponsoredAssetMinFee = Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), token))
    sender.grpc.broadcastSponsorFee(sponsor, sponsoredAssetMinFee, fee = sponsorFee, waitForTx = true)

    val sponsoredAssetUpdatedMinFee = Some(Amount(ByteString.copyFrom(Base58.decode(sponsoredAssetId)), largeFee))
    sender.grpc.broadcastSponsorFee(sponsor, sponsoredAssetUpdatedMinFee, fee = sponsorFee, waitForTx = true)

    assertGrpcError(
      sender.grpc.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, smallFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true),
      s"does not exceed minimal value of $minFee WAVES or $largeFee $sponsoredAssetId",
      Code.INVALID_ARGUMENT
    )
    val aliceWavesBalance = sender.grpc.wavesBalance(aliceAddress)
    val bobWavesBalance = sender.grpc.wavesBalance(bobAddress)
    val sponsorWavesBalance = sender.grpc.wavesBalance(sponsorAddress)
    val aliceAssetBalance = sender.grpc.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
    val bobAssetBalance = sender.grpc.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)
    val sponsorAssetBalance = sender.grpc.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L)

    sender.grpc.broadcastTransfer(alice, Recipient().withPublicKeyHash(bobAddress), 10 * token, largeFee, assetId = sponsoredAssetId, feeAssetId = sponsoredAssetId, waitForTx = true)

    sender.grpc.wavesBalance(aliceAddress).available shouldBe aliceWavesBalance.available
    sender.grpc.wavesBalance(bobAddress).available shouldBe bobWavesBalance.available
    sender.grpc.wavesBalance(sponsorAddress).available shouldBe sponsorWavesBalance.available - FeeValidation.FeeUnit * largeFee / largeFee
    sender.grpc.assetsBalance(aliceAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe aliceAssetBalance - 10 * token - largeFee
    sender.grpc.assetsBalance(bobAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe bobAssetBalance + 10 * token
    sender.grpc.assetsBalance(sponsorAddress, Seq(sponsoredAssetId)).getOrElse(sponsoredAssetId, 0L) shouldBe sponsorAssetBalance + largeFee
  }
}
