package com.wavesplatform.http

import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.{TestWallet, TransactionGen}
import org.scalacheck.Shrink
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, Json}
import scorex.api.http.assets.TransferRequest
import scorex.api.http.{ApiKeyNotValid, PaymentApiRoute}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{NewTransactionHandler, Transaction, ValidationError}
import scorex.utils.NTP

class PaymentRouteSpec extends RouteSpec("/payment")
  with MockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with TransactionGen {


  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  "accepts payments" in {
    forAll(accountOrAliasGen.label("recipient"), positiveLongGen.label("amount"), smallFeeGen.label("fee")) {
      case (recipient, amount, fee) =>

        val sender = testWallet.privateKeyAccounts().head
        val tx = TransferTransaction.create(None, sender, recipient, amount, System.currentTimeMillis(), None, fee, Array())
        val stmMock: NewTransactionHandler = {

          def alwaysTx(t: Transaction): Either[ValidationError, Transaction] = tx

          val m = mock[NewTransactionHandler]
          (m.onNewTransaction(_: Transaction))
            .expects(*)
            .onCall(alwaysTx _)
            .anyNumberOfTimes()
          m
        }

        val route = PaymentApiRoute(restAPISettings, testWallet, stmMock, NTP).route

        val req = Json.obj("sender" -> sender.address, "recipient" -> recipient.stringRepr, "amount" -> amount, "fee" -> fee)
        val transferReq = TransferRequest(None, None, amount, fee, sender.address, None, recipient.stringRepr)

        Post(routePath(""), req) ~> route should produce(ApiKeyNotValid)
        Post(routePath(""), req) ~> api_key(apiKey) ~> route ~> check {
          val resp = responseAs[JsObject]

          (resp \ "assetId").asOpt[String] shouldEqual None
          (resp \ "feeAsset").asOpt[String] shouldEqual None
          (resp \ "type").as[Int] shouldEqual 4
          (resp \ "fee").as[Int] shouldEqual fee
          (resp \ "amount").as[Long] shouldEqual amount
          (resp \ "timestamp").as[Long] shouldEqual tx.right.get.timestamp
          (resp \ "signature").as[String] shouldEqual tx.right.get.signature.base58
          (resp \ "sender").as[String] shouldEqual sender.address
          (resp \ "recipient").as[String] shouldEqual recipient.stringRepr
        }
    }
  }
}
