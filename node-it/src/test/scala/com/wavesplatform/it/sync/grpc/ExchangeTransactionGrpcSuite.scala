package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync.{matcherFee, minFee, someAssetAmount}
import com.wavesplatform.test._
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils._
import io.grpc.Status.Code

import scala.collection.immutable

class ExchangeTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime {

  val transactionV1versions: (TxVersion, TxVersion, TxVersion) = (1: Byte, 1: Byte, 1: Byte)
  val transactionV2versions: immutable.Seq[(TxVersion, TxVersion, TxVersion)] = for {
    o1ver <- 1 to 3
    o2ver <- 1 to 3
    txVer <- 2 to 3
  } yield (o1ver.toByte, o2ver.toByte, txVer.toByte)

  val (buyer, buyerAddress)     = (firstAcc, firstAddress)
  val (seller, sellerAddress)   = (secondAcc, secondAddress)
  val (matcher, matcherAddress) = (thirdAcc, thirdAddress)

  val versions: immutable.Seq[(TxVersion, TxVersion, TxVersion)] = transactionV1versions +: transactionV2versions

  test("exchange tx with orders v1,v2") {
    val exchAsset =
      sender.broadcastIssue(buyer, Base64.encode("exchAsset".utf8Bytes), someAssetAmount, 8, reissuable = true, 1.waves, waitForTx = true)
    val exchAssetId        = PBTransactions.vanilla(exchAsset, unsafe = false).explicitGet().id().toString
    val price              = 500000L
    val amount             = 40000000L
    val priceAssetSpending = amount * price / 100000000L
    val pair               = AssetPair.createAssetPair("WAVES", exchAssetId).get
    for ((o1ver, o2ver, tver) <- versions) {
      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime / 2
      val buy                 = Order.buy(o1ver, buyer, matcher.publicKey, pair, amount, price, ts, expirationTimestamp, matcherFee).explicitGet()
      val sell                = Order.sell(o2ver, seller, matcher.publicKey, pair, amount, price, ts, expirationTimestamp, matcherFee).explicitGet()
      val buyerWavesBalanceBefore  = sender.wavesBalance(buyerAddress).available
      val sellerWavesBalanceBefore = sender.wavesBalance(sellerAddress).available
      val buyerAssetBalanceBefore  = sender.assetsBalance(buyerAddress, Seq(exchAssetId)).getOrElse(exchAssetId, 0L)
      val sellerAssetBalanceBefore = sender.assetsBalance(sellerAddress, Seq(exchAssetId)).getOrElse(exchAssetId, 0L)

      sender.exchange(matcher, buy, sell, amount, price, matcherFee, matcherFee, matcherFee, ts, tver, waitForTx = true)

      sender.wavesBalance(buyerAddress).available shouldBe buyerWavesBalanceBefore + amount - matcherFee
      sender.wavesBalance(sellerAddress).available shouldBe sellerWavesBalanceBefore - amount - matcherFee
      sender.assetsBalance(buyerAddress, Seq(exchAssetId))(exchAssetId) shouldBe buyerAssetBalanceBefore - priceAssetSpending
      sender.assetsBalance(sellerAddress, Seq(exchAssetId))(exchAssetId) shouldBe sellerAssetBalanceBefore + priceAssetSpending
    }
  }

  test("exchange tx with orders v3") {
    val feeAsset           = sender.broadcastIssue(buyer, "feeAsset", someAssetAmount, 8, reissuable = true, 1.waves, waitForTx = true)
    val feeAssetId         = PBTransactions.vanilla(feeAsset, unsafe = false).explicitGet().id()
    val price              = 500000L
    val amount             = 40000000L
    val priceAssetSpending = price * amount / 100000000L

    sender.broadcastTransfer(
      buyer,
      Recipient().withPublicKeyHash(sellerAddress),
      someAssetAmount / 2,
      minFee,
      assetId = feeAssetId.toString,
      waitForTx = true
    )

    for (
      (o1ver, o2ver, matcherFeeOrder1, matcherFeeOrder2, buyerWavesDelta, sellerWavesDelta, buyerAssetDelta, sellerAssetDelta) <- Seq(
        (1: Byte, 3: Byte, Waves, IssuedAsset(feeAssetId), amount - matcherFee, -amount, -priceAssetSpending, priceAssetSpending - matcherFee),
        (1: Byte, 3: Byte, Waves, Waves, amount - matcherFee, -amount - matcherFee, -priceAssetSpending, priceAssetSpending),
        (2: Byte, 3: Byte, Waves, IssuedAsset(feeAssetId), amount - matcherFee, -amount, -priceAssetSpending, priceAssetSpending - matcherFee),
        (3: Byte, 1: Byte, IssuedAsset(feeAssetId), Waves, amount, -amount - matcherFee, -priceAssetSpending - matcherFee, priceAssetSpending),
        (2: Byte, 3: Byte, Waves, Waves, amount - matcherFee, -amount - matcherFee, -priceAssetSpending, priceAssetSpending),
        (3: Byte, 2: Byte, IssuedAsset(feeAssetId), Waves, amount, -amount - matcherFee, -priceAssetSpending - matcherFee, priceAssetSpending)
      )
    ) {
      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        sender.broadcastTransfer(
          buyer,
          Recipient().withPublicKeyHash(sellerAddress),
          100000,
          minFee,
          assetId = feeAssetId.toString,
          waitForTx = true
        )
      }

      val buyerWavesBalanceBefore  = sender.wavesBalance(buyerAddress).available
      val sellerWavesBalanceBefore = sender.wavesBalance(sellerAddress).available
      val buyerAssetBalanceBefore  = sender.assetsBalance(buyerAddress, Seq(feeAssetId.toString)).getOrElse(feeAssetId.toString, 0L)
      val sellerAssetBalanceBefore = sender.assetsBalance(sellerAddress, Seq(feeAssetId.toString)).getOrElse(feeAssetId.toString, 0L)

      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime / 2
      val assetPair           = AssetPair.createAssetPair("WAVES", feeAssetId.toString).get
      val buy =
        Order.buy(o1ver, buyer, matcher.publicKey, assetPair, amount, price, ts, expirationTimestamp, matcherFee, matcherFeeOrder1).explicitGet()
      val sell =
        Order.sell(o2ver, seller, matcher.publicKey, assetPair, amount, price, ts, expirationTimestamp, matcherFee, matcherFeeOrder2).explicitGet()

      sender.exchange(matcher, sell, buy, amount, price, matcherFee, matcherFee, matcherFee, ts, 3, waitForTx = true)

      sender.wavesBalance(buyerAddress).available shouldBe (buyerWavesBalanceBefore + buyerWavesDelta)
      sender.wavesBalance(sellerAddress).available shouldBe (sellerWavesBalanceBefore + sellerWavesDelta)
      sender.assetsBalance(buyerAddress, Seq(feeAssetId.toString))(feeAssetId.toString) shouldBe (buyerAssetBalanceBefore + buyerAssetDelta)
      sender.assetsBalance(sellerAddress, Seq(feeAssetId.toString))(feeAssetId.toString) shouldBe (sellerAssetBalanceBefore + sellerAssetDelta)
    }
  }

  test("cannot exchange non-issued assets") {
    val exchAsset: IssueTransaction = IssueTransaction
      .selfSigned(
        TxVersion.V1,
        sender.keyPair,
        "myasset",
        "my asset description",
        quantity = someAssetAmount,
        decimals = 2,
        reissuable = true,
        script = None,
        fee = 1.waves,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()
    for ((o1ver, o2ver, tver) <- versions) {

      val assetId             = exchAsset.id().toString
      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime / 2
      val price               = 2 * Order.PriceConstant
      val amount              = 1
      val pair                = AssetPair.createAssetPair("WAVES", assetId).get
      val buy                 = Order.buy(o1ver, buyer, matcher.publicKey, pair, amount, price, ts, expirationTimestamp, matcherFee).explicitGet()
      val sell                = Order.sell(o2ver, seller, matcher.publicKey, pair, amount, price, ts, expirationTimestamp, matcherFee).explicitGet()

      assertGrpcError(
        sender.exchange(matcher, buy, sell, amount, price, matcherFee, matcherFee, matcherFee, ts, tver),
        "Assets should be issued before they can be traded",
        Code.INVALID_ARGUMENT
      )
    }
  }
}
