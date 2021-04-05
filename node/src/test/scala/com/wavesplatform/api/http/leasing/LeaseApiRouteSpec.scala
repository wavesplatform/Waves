package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity}
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, LeaseInfo}
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.http.{RestAPISettingsHelper, RouteSpec}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.SystemTime
import com.wavesplatform.wallet.Wallet
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsArray, JsObject, Json}

//noinspection TypeAnnotation
class LeaseApiRouteSpec extends RouteSpec("/leasing") with PathMockFactory with RestAPISettingsHelper {
  val blockchain = stub[Blockchain]
  val commonApi  = stub[CommonAccountsApi]

  val route = LeaseApiRoute(restAPISettings, stub[Wallet], blockchain, stub[TransactionPublisher], SystemTime, commonApi).route

  routePath("/info") in {
    val lease       = TxHelpers.lease()
    val leaseCancel = TxHelpers.leaseCancel(lease.id())
    (blockchain.transactionInfo _).when(lease.id()).returning(Some((1, lease, true)))
    (commonApi.leaseInfo _)
      .when(lease.id())
      .returning(
        Some(
          LeaseInfo(lease.id(), lease.id(), lease.sender.toAddress, lease.recipient.asInstanceOf[Address], lease.amount, 1, LeaseInfo.Status.Active)
        )
      )
    (commonApi.leaseInfo _).when(*).returning(None)

    Get(routePath(s"/info/${lease.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBeJson s"""
                               |{
                               |  "leaseId" : "${lease.id()}",
                               |  "originTransactionId" : "${lease.id()}",
                               |  "sender" : "${lease.sender.toAddress}",
                               |  "recipient" : "${lease.recipient.stringRepr}",
                               |  "amount" : 1000000000,
                               |  "height" : 1,
                               |  "status" : "active",
                               |  "leaseTransactionRef" : {
                               |    "originTransactionId" : "${lease.id()}",
                               |    "height" : 1
                               |  },
                               |  "leaseCancelTransactionRef" : {
                               |    "originTransactionId" : "${leaseCancel.id()}",
                               |    "height" : 2
                               |  }
                               |}
                               |""".stripMargin
    }

    val leasesListJson = Json.parse(s"""[
                                       |{
                                       |  "leaseId" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "${lease.sender.toAddress}",
                                       |  "recipient" : "${lease.recipient.stringRepr}",
                                       |  "amount" : 1000000000,
                                       |  "height" : 1,
                                       |  "status" : "active",
                                       |  "leaseTransactionRef" : {
                                       |    "originTransactionId" : "${lease.id()}",
                                       |    "height" : 1
                                       |  },
                                       |  "leaseCancelTransactionRef" : {
                                       |    "originTransactionId" : "${leaseCancel.id()}",
                                       |    "height" : 2
                                       |  }
                                       |},
                                       |{
                                       |  "leaseId" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "${lease.sender.toAddress}",
                                       |  "recipient" : "${lease.recipient.stringRepr}",
                                       |  "amount" : 1000000000,
                                       |  "height" : 1,
                                       |  "status" : "active",
                                       |  "leaseTransactionRef" : {
                                       |    "originTransactionId" : "${lease.id()}",
                                       |    "height" : 1
                                       |  },
                                       |  "leaseCancelTransactionRef" : {
                                       |    "originTransactionId" : "${leaseCancel.id()}",
                                       |    "height" : 2
                                       |  }
                                       |}
                                       |]
                                       |""".stripMargin)

    Get(routePath(s"/info?id=${lease.id()}&id=${lease.id()}")) ~> route ~> check {
      val response = responseAs[JsArray]
      response shouldBeJson leasesListJson
    }

    Post(
      routePath(s"/info"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Seq(lease.id().toString, lease.id().toString)).toString())
    ) ~> route ~> check {
      val response = responseAs[JsArray]
      response shouldBeJson leasesListJson
    }

    Post(
      routePath(s"/info"),
      FormData("id" -> lease.id().toString, "id" -> lease.id().toString)
    ) ~> route ~> check {
      val response = responseAs[JsArray]
      response shouldBeJson leasesListJson
    }

    Get(routePath(s"/info?id=nonvalid&id=${leaseCancel.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBeJson s"""
                               |{
                               |  "error" : 116,
                               |  "message" : "Request contains invalid IDs. nonvalid, ${leaseCancel.id()}",
                               |  "ids" : [ "nonvalid", "${leaseCancel.id()}" ]
                               |}""".stripMargin
    }
  }
}
