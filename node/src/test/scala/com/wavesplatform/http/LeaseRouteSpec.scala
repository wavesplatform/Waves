package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.Diff
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

  private def checkDetailsAgainstTransaction(leaseTx: LeaseTransaction, details: JsObject): Unit = {
    (details \ "leaseId").as[ByteStr] shouldEqual leaseTx.id()
    (details \ "originTransactionId").as[ByteStr] shouldEqual leaseTx.id()
    (details \ "sender").as[String] shouldEqual leaseTx.sender.toAddress.toString
    (details \ "amount").as[Long] shouldEqual leaseTx.amount
  }

  "returns active leases" - {
    "created by lease transaction" in forAll(genesisWithLease) {
      case (genesis, leaseTransaction) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis)
          d.appendBlock(leaseTransaction)
          Get(routePath(s"/active/${leaseTransaction.sender.toAddress}")) ~> r ~> check {
            val resp = responseAs[Seq[JsObject]]
            resp.size shouldEqual 1
            checkDetailsAgainstTransaction(leaseTransaction, resp.head)
          }

        }

    }
    "created by invoke script transaction" in {}
  }

  "does not return already canceled leases" - {
    "created and cancelled by Lease/LeaseCancel transactions" in {}
    "created bo LeaseTransaction and canceled by InvokeScriptTransaction" in {}
    "created by InvokeScriptTransaction and canceled by CancelLeaseTransaction" in {}
    "created and canceled by InvokeScriptTransaction" in {}
  }
}
