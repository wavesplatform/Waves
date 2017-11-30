package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import scorex.api.http.assets.PaymentRequest

import scala.concurrent.Await
import scala.concurrent.duration._

class PaymentTransactionSuite extends BaseTransactionSuite {

  test("waves payment changes waves balances and eff.b.") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)
        .zip(assertBalances(secondAddress, 100.waves, 100.waves))

      transferId <- sender.payment(firstAddress, secondAddress, 5.waves, fee = 5.waves).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(transferId, 1)
      _ <- assertBalances(firstAddress, 90.waves, 90.waves)
        .zip(assertBalances(secondAddress, 105.waves, 105.waves))
    } yield succeed

    Await.result(f, 2.minute)
  }

  test("obsolete endpoints should send BadRequest") {
    val payment = PaymentRequest(5.waves, 1.waves, firstAddress, secondAddress)
    val errorMessage = "This API is no longer supported"
    val f = for {
      _ <- assertBadRequestAndMessage(sender.postJson("/waves/payment/signature", payment), errorMessage)
      _ <- assertBadRequestAndMessage(sender.postJson("/waves/create-signed-payment", payment), errorMessage)
      _ <- assertBadRequestAndMessage(sender.postJson("/waves/external-payment", payment), errorMessage)
      _ <- assertBadRequestAndMessage(sender.postJson("/waves/broadcast-signed-payment", payment), errorMessage)
    } yield succeed

    Await.result(f, 1.minute)
  }
}
