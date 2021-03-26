package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.Diff
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.{NTPTime, NoShrink, TestWallet, TransactionGen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsObject

import scala.concurrent.Future

class LeaseRouteSpec
    extends RouteSpec("/leasing")
    with ScalaCheckPropertyChecks
    with TransactionGen
    with RestAPISettingsHelper
    with NoShrink
    with NTPTime
    with WithDomain
    with TestWallet {
  private def route(domain: Domain) =
    LeaseApiRoute(
      restAPISettings,
      testWallet,
      domain.blockchain,
      (_, _) => Future.successful(TracedResult(Right(true))),
      ntpTime,
      CommonAccountsApi(domain.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), domain.db, domain.blockchain)
    )

  private def withRoute(f: (Domain, Route) => Unit): Unit =
    withDomain(domainSettingsWithFeatures(BlockchainFeatures.implemented.flatMap(BlockchainFeatures.feature).toSeq: _*)) { d =>
      f(d, route(d).route)
    }

  private val genesisWithLease = for {
    sender  <- accountGen
    genesis <- genesisGeneratorP(sender.toAddress)
    leaseTx <- leaseGen(sender, ntpTime.correctedTime())
  } yield (genesis, leaseTx)

  private def checkDetails(id: ByteStr, details: LeaseDetails, json: JsObject): Unit = {
    (json \ "leaseId").as[ByteStr] shouldEqual id
    (json \ "originTransactionId").as[ByteStr] shouldEqual details.sourceId
    (json \ "sender").as[String] shouldEqual details.sender.toAddress.toString
    (json \ "amount").as[Long] shouldEqual details.amount
  }

  private def checkActiveLeasesFor(address: AddressOrAlias, route: Route, expectedDetails: Seq[(ByteStr, LeaseDetails)]): Unit =
    Get(routePath(s"/active/$address")) ~> route ~> check {
      val resp = responseAs[Seq[JsObject]]
      resp.size shouldEqual expectedDetails.size
      resp.zip(expectedDetails).foreach {
        case (json, (id, details)) => checkDetails(id, details, json)
      }
    }

  private def toDetails(lt: LeaseTransaction) = LeaseDetails(lt.sender, lt.recipient, lt.id(), lt.amount, isActive = true)

  "returns active leases" - {
    "created by lease transaction" in forAll(genesisWithLease) {
      case (genesis, leaseTransaction) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis)
          // check liquid block
          d.appendBlock(leaseTransaction)
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq(leaseTransaction.id() -> toDetails(leaseTransaction)))
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq(leaseTransaction.id() -> toDetails(leaseTransaction)))
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq(leaseTransaction.id() -> toDetails(leaseTransaction)))
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq(leaseTransaction.id() -> toDetails(leaseTransaction)))
        }
    }
    "created by invoke script transaction" in {

    }
  }

  "does not return already canceled leases" - {
    "created and cancelled by Lease/LeaseCancel transactions" in {}
    "created bo LeaseTransaction and canceled by InvokeScriptTransaction" in {}
    "created by InvokeScriptTransaction and canceled by CancelLeaseTransaction" in {}
    "created and canceled by InvokeScriptTransaction" in {}
  }
}
