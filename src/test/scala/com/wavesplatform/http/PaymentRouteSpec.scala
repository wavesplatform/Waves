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
import scorex.transaction.TransactionOperations
import scorex.transaction.assets.TransferTransaction
import scorex.wallet.Wallet

class PaymentRouteSpec extends RouteSpec("/payment")
  with MockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with TransactionGen {
  private val txOps = mock[TransactionOperations]
  private val route = PaymentApiRoute(restAPISettings, testWallet, txOps).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  "accepts payments" in {
    forAll(accountGen.label("sender"), accountOrAliasGen.label("recipient"), positiveLongGen.label("amount"), smallFeeGen.label("fee")) {
      case (sender, recipient, amount, fee) =>
        val req =
          Json.obj("sender" -> sender.address, "recipient" -> recipient.stringRepr, "amount" -> amount, "fee" -> fee)

        val transferReq = TransferRequest(None, None, amount, fee, sender.address, None, recipient.stringRepr)
        val tx = TransferTransaction.create(None, sender, recipient, amount, System.currentTimeMillis(), None, fee, Array())

        (txOps.transferAsset(_: TransferRequest, _: Wallet)).expects(transferReq, testWallet).returning(tx).once()
        Post(routePath(""), req) ~> route should produce(ApiKeyNotValid)
        Post(routePath(""), req) ~> api_key(apiKey) ~> route ~> check {
          val resp = responseAs[JsObject]

          (resp \ "assetId").asOpt[String] shouldEqual None
          (resp \ "feeAsset").asOpt[String] shouldEqual None
          (resp \ "type").as[Int] shouldEqual 4
          (resp \ "fee").as[Int] shouldEqual fee
          (resp \ "amount").as[Long] shouldEqual amount
          (resp \ "timestamp").as[Long] shouldEqual tx.right.get.timestamp
          (resp \ "signature").as[String] shouldEqual Base58.encode(tx.right.get.signature)
          (resp \ "sender").as[String] shouldEqual sender.address
          (resp \ "recipient").as[String] shouldEqual recipient.stringRepr
        }
    }
  }
}
