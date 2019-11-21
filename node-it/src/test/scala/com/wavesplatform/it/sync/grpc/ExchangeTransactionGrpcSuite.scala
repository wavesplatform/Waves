package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{matcherFee, minFee, someAssetAmount}
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import io.grpc.Status.Code

class ExchangeTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime {

  val transactionV1versions = (1: Byte, 1: Byte, 1: Byte)
  val transactionV2versions = for {
    o1ver <- 1 to 3
    o2ver <- 1 to 3
  } yield (o1ver.toByte, o2ver.toByte, 2.toByte)

  val (buyer, buyerAddress)     = (firstAcc, firstAddress)
  val (seller, sellerAddress)   = (secondAcc, secondAddress)
  val (matcher, matcherAddress) = (thirdAcc, thirdAddress)

  val versions = transactionV1versions +: transactionV2versions

  test("exchange tx with orders v1,v2") {
    val exchAsset          = sender.grpc.broadcastIssue(buyer, "exchAsset", someAssetAmount, 8, true, 1.waves, waitForTx = true)
    val exchAssetId        = PBTransactions.vanilla(exchAsset).explicitGet().id().toString
    val price              = 500000L
    val amount             = 40000000L
    val priceAssetSpending = amount * price / 100000000L
    val pair               = AssetPair.createAssetPair("WAVES", exchAssetId).get
    for ((o1ver, o2ver, tver) <- versions) {
      val ts                       = ntpTime.correctedTime()
      val expirationTimestamp      = ts + Order.MaxLiveTime
      val buy                      = Order.buy(o1ver, buyer, matcher, pair, amount, price, ts, expirationTimestamp, matcherFee)
      val sell                     = Order.sell(o2ver, seller, matcher, pair, amount, price, ts, expirationTimestamp, matcherFee)
      val buyerWavesBalanceBefore  = sender.grpc.wavesBalance(buyerAddress).available
      val sellerWavesBalanceBefore = sender.grpc.wavesBalance(sellerAddress).available
      val buyerAssetBalanceBefore  = sender.grpc.assetsBalance(buyerAddress, Seq(exchAssetId)).getOrElse(exchAssetId, 0L)
      val sellerAssetBalanceBefore = sender.grpc.assetsBalance(sellerAddress, Seq(exchAssetId)).getOrElse(exchAssetId, 0L)

      sender.grpc.exchange(matcher, buy, sell, amount, price, matcherFee, matcherFee, matcherFee, ts, tver, waitForTx = true)

      sender.grpc.wavesBalance(buyerAddress).available shouldBe buyerWavesBalanceBefore + amount - matcherFee
      sender.grpc.wavesBalance(sellerAddress).available shouldBe sellerWavesBalanceBefore - amount - matcherFee
      sender.grpc.assetsBalance(buyerAddress, Seq(exchAssetId))(exchAssetId) shouldBe buyerAssetBalanceBefore - priceAssetSpending
      sender.grpc.assetsBalance(sellerAddress, Seq(exchAssetId))(exchAssetId) shouldBe sellerAssetBalanceBefore + priceAssetSpending
    }
  }

  test("exchange tx with orders v3") {
    val feeAsset           = sender.grpc.broadcastIssue(buyer, "feeAsset", someAssetAmount, 8, true, 1.waves, waitForTx = true)
    val feeAssetId         = PBTransactions.vanilla(feeAsset).explicitGet().id()
    val price              = 500000L
    val amount             = 40000000L
    val priceAssetSpending = price * amount / 100000000L

    for ((o1ver, o2ver, matcherFeeOrder1, matcherFeeOrder2, buyerWavesDelta, sellerWavesDelta, buyerAssetDelta, sellerAssetDelta) <- Seq(
           (1: Byte, 3: Byte, Waves, IssuedAsset(feeAssetId), amount - matcherFee, -amount, -priceAssetSpending, priceAssetSpending - matcherFee),
           (1: Byte, 3: Byte, Waves, Waves, amount - matcherFee, -amount - matcherFee, -priceAssetSpending, priceAssetSpending),
           (2: Byte, 3: Byte, Waves, IssuedAsset(feeAssetId), amount - matcherFee, -amount, -priceAssetSpending, priceAssetSpending - matcherFee),
           (3: Byte, 1: Byte, IssuedAsset(feeAssetId), Waves, amount, -amount - matcherFee, -priceAssetSpending - matcherFee, priceAssetSpending),
           (2: Byte, 3: Byte, Waves, Waves, amount - matcherFee, -amount - matcherFee, -priceAssetSpending, priceAssetSpending),
           (3: Byte, 2: Byte, IssuedAsset(feeAssetId), Waves, amount, -amount - matcherFee, -priceAssetSpending - matcherFee, priceAssetSpending)
         )) {
      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        sender.grpc.broadcastTransfer(buyer, Recipient().withAddress(sellerAddress), 100000, minFee, assetId = feeAssetId.toString, waitForTx = true)
      }

      val buyerWavesBalanceBefore  = sender.grpc.wavesBalance(buyerAddress).available
      val sellerWavesBalanceBefore = sender.grpc.wavesBalance(sellerAddress).available
      val buyerAssetBalanceBefore  = sender.grpc.assetsBalance(buyerAddress, Seq(feeAssetId.toString)).getOrElse(feeAssetId.toString, 0L)
      val sellerAssetBalanceBefore = sender.grpc.assetsBalance(sellerAddress, Seq(feeAssetId.toString)).getOrElse(feeAssetId.toString, 0L)

      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime
      val assetPair           = AssetPair.createAssetPair("WAVES", feeAssetId.toString).get
      val buy                 = Order.buy(o1ver, buyer, matcher, assetPair, amount, price, ts, expirationTimestamp, matcherFee, matcherFeeOrder1)
      val sell                = Order.sell(o2ver, seller, matcher, assetPair, amount, price, ts, expirationTimestamp, matcherFee, matcherFeeOrder2)

      sender.grpc.exchange(matcher, buy, sell, amount, price, matcherFee, matcherFee, matcherFee, ts, 2, waitForTx = true)

      sender.grpc.wavesBalance(buyerAddress).available shouldBe (buyerWavesBalanceBefore + buyerWavesDelta)
      sender.grpc.wavesBalance(sellerAddress).available shouldBe (sellerWavesBalanceBefore + sellerWavesDelta)
      sender.grpc.assetsBalance(buyerAddress, Seq(feeAssetId.toString))(feeAssetId.toString) shouldBe (buyerAssetBalanceBefore + buyerAssetDelta)
      sender.grpc.assetsBalance(sellerAddress, Seq(feeAssetId.toString))(feeAssetId.toString) shouldBe (sellerAssetBalanceBefore + sellerAssetDelta)
    }
  }

  test("cannot exchange non-issued assets") {
    val exchAsset: IssueTransaction = IssueTransaction
      .selfSigned(
        TxVersion.V1,
        sender = sender.privateKey,
        name = "myasset",
        description = "my asset description",
        quantity = someAssetAmount,
        decimals = 2,
        reissuable = true,
        script = None,
        fee = 1.waves,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get
    for ((o1ver, o2ver, tver) <- versions) {

      val assetId             = exchAsset.id().toString
      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime
      val price               = 2 * Order.PriceConstant
      val amount              = 1
      val pair                = AssetPair.createAssetPair("WAVES", assetId).get
      val buy                 = Order.buy(o1ver, buyer, matcher, pair, amount, price, ts, expirationTimestamp, matcherFee)
      val sell                = Order.sell(o2ver, seller, matcher, pair, amount, price, ts, expirationTimestamp, matcherFee)

      assertGrpcError(
        sender.grpc.exchange(matcher, buy, sell, amount, price, matcherFee, matcherFee, matcherFee, ts, tver),
        "Assets should be issued before they can be traded",
        Code.INVALID_ARGUMENT
      )
    }
  }
}
