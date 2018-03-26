package com.wavesplatform.it.async.transactions

import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._

import scala.concurrent.Await
import scala.concurrent.duration._

class ReissueTransactionSuite extends BaseTransactionSuite {

  private val waitCompletion  = 2.minutes
  private val defaultQuantity = 100000
  private val issueFee        = 3.waves

  test("asset reissue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      (balance, effectiveBalance) <- notMiner.accountBalances(firstAddress)

      issuedAssetId <- sender.issue(firstAddress, "name2", "description2", defaultQuantity, decimals = 2, reissuable = true, fee = issueFee).map(_.id)
      _             <- nodes.waitForHeightAraiseAndTxPresent(issuedAssetId)
      _ <- notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee) zip notMiner.assertAssetBalance(firstAddress,
                                                                                                                                  issuedAssetId,
                                                                                                                                  defaultQuantity)

      reissueTxId <- sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = issueFee).map(_.id)
      _           <- nodes.waitForHeightAraiseAndTxPresent(reissueTxId)
      _ <- notMiner
        .assertBalances(firstAddress, balance - 2 * issueFee, effectiveBalance - 2 * issueFee)
        .zip(notMiner.assertAssetBalance(firstAddress, issuedAssetId, 2 * defaultQuantity))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can't reissue not reissuable asset") {
    val f = for {
      (balance, effectiveBalance) <- notMiner.accountBalances(firstAddress)

      issuedAssetId <- sender.issue(firstAddress, "name2", "description2", defaultQuantity, decimals = 2, reissuable = false, issueFee).map(_.id)
      _             <- nodes.waitForHeightAraiseAndTxPresent(issuedAssetId)
      _ <- notMiner
        .assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
        .zip(notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity))

      _ <- assertBadRequestAndMessage(sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = issueFee),
                                      "Asset is not reissuable")
      _ <- nodes.waitForHeightAraise()

      _ <- notMiner
        .assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
        .zip(notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee))

    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("not able to reissue if cannot pay fee - insufficient funds") {
    val f = for {
      (balance, effectiveBalance) <- notMiner.accountBalances(firstAddress)
      reissueFee = effectiveBalance + 1.waves

      issuedAssetId <- sender.issue(firstAddress, "name3", "description3", defaultQuantity, decimals = 2, reissuable = true, issueFee).map(_.id)

      _ <- nodes.waitForHeightAraiseAndTxPresent(issuedAssetId)

      _ <- assertBadRequestAndMessage(sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = reissueFee),
                                      "negative waves balance")
      _ <- nodes.waitForHeightAraise()

      _ <- notMiner
        .assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
        .zip(notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee))
    } yield succeed

    Await.result(f, waitCompletion)
  }

}
