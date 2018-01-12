package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._

import scala.concurrent.Await
import scala.concurrent.duration._

class ReissueTransactionSuite extends BaseTransactionSuite {

  private val waitCompletion = 2.minutes
  private val defaultQuantity = 100000
  private val issueFee = 3.waves

  test("asset reissue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      (balance, effectiveBalance) <- accountBalances(firstAddress)

      issuedAssetId <- sender.issue(firstAddress, "name2", "description2", defaultQuantity, decimals = 2, reissuable = true, fee = issueFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)
      _ <- assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee) zip assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

      reissueTxId <- sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = issueFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(reissueTxId, 1)
      _ <- assertBalances(firstAddress, balance - 2 * issueFee, effectiveBalance - 2 * issueFee)
        .zip(assertAssetBalance(firstAddress, issuedAssetId, 2 * defaultQuantity))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can't reissue not reissuable asset") {
    val f = for {
      (balance, effectiveBalance) <- accountBalances(firstAddress)

      issuedAssetId <- sender.issue(firstAddress, "name2", "description2", defaultQuantity, decimals = 2, reissuable = false, issueFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)
      _ <- assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
        .zip(assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity))

      _ <- assertBadRequestAndMessage(
        sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = issueFee), "Asset is not reissuable")
      _ <- waitForHeightAraise(1)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
        .zip(assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee))

    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("not able to reissue if cannot pay fee - insufficient funds") {
    val f = for {
      (balance, effectiveBalance) <- accountBalances(firstAddress)
      reissueFee = effectiveBalance + 1.waves

      issuedAssetId <- sender.issue(firstAddress, "name3", "description3", defaultQuantity, decimals = 2, reissuable = true, issueFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)

      _ <- assertBadRequestAndMessage(
        sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = reissueFee), "negative waves balance")
      _ <- waitForHeightAraise(1)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
        .zip(assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee))
    } yield succeed

    Await.result(f, waitCompletion)
  }


}
