package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration._

class IssueTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  private val waitCompletion = 2.minutes
  private val defaultQuantity = 100000
  private val assetFee = 5.waves

  test("asset issue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val assetName = "myasset"
    val assetDescription = "my asset description"
    val f = for {

      (firstAddressBalance, firstAddressEffectiveBalance) <- accountBalance(firstAddress).zip(accountEffectiveBalance(firstAddress))

      issuedAssetId <- sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = true, assetFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)

      _ <- assertBalances(firstAddress, firstAddressBalance - assetFee, firstAddressEffectiveBalance - assetFee)
        .zip(assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("Able to create asset with the same name") {
    val assetName = "myasset"
    val assetDescription = "my asset description"
    val f = for {

      (firstAddressBalance, firstAddressEffectiveBalance) <- accountBalance(firstAddress).zip(accountEffectiveBalance(firstAddress))

      issuedAssetId <- sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = false, assetFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)

      issuedAssetId <- sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = true, assetFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
        .zip(assertBalances(firstAddress, firstAddressBalance - 2 * assetFee, firstAddressEffectiveBalance - 2 * assetFee))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("Not able to create asset when insufficient funds") {
    val assetName = "myasset"
    val assetDescription = "my asset description"
    val f = for {

      firstAddressEffectiveBalance <- accountEffectiveBalance(firstAddress)
      bigAssetFee = firstAddressEffectiveBalance + 1.waves

      _ <- assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = false, bigAssetFee),
        "negative waves balance")
    } yield succeed

    Await.result(f, waitCompletion)
  }

  val invalidAssetValue =
    Table(("assetVal", "decimals", "message"),
      (0l, 2, "negative amount"),
      (1l, 9, "Too big sequences requested"),
      (-1l, 1, "negative amount"),
      (1l, -1, "Too big sequences requested")) //??? what message shoild be?

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, message: String) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      val assetName = "myasset2"
      val assetDescription = "my asset description 2"
      val decimalBytes: Byte = decimals.toByte
      val f = for {
        _ <- assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, assetVal, decimalBytes, reissuable = false, assetFee),
          message)
      } yield succeed

      Await.result(f, waitCompletion)
    }
  }

}
