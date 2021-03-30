package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.{AddressOrAlias, KeyPair}
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{BinaryDataEntry, Diff}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, TxVersion}
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

  private def setScriptTransaction(sender: KeyPair) =
    SetScriptTransaction
      .selfSigned(
        TxVersion.V2,
        sender,
        Some(TestCompiler(V5).compileContract("""
          |{-# STDLIB_VERSION 4 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |
          |@Callable(inv)
          |func leaseTo(recipient: ByteVector, amount: Int) = {
          |  let lease = Lease(Address(recipient), amount)
          |  [
          |    lease,
          |    BinaryEntry("leaseId", lease.calculateLeaseId())
          |  ]
          |}
          |
          |@Callable(inv)
          |func cancelLease(id: ByteVector) = {
          |  [
          |    LeaseCancel(id)
          |  ]
          |}
          |""".stripMargin)),
        0.01.waves,
        ntpTime.getTimestamp()
      )
      .explicitGet()

  private def invokeLeaseCancel(sender: KeyPair, leaseId: ByteStr) =
    InvokeScriptTransaction
      .selfSigned(
        TxVersion.V2,
        sender,
        sender.toAddress,
        Some(
          FUNCTION_CALL(
            FunctionHeader.User("cancelLease"),
            List(CONST_BYTESTR(leaseId).explicitGet())
          )
        ),
        Seq.empty,
        0.005.waves,
        Asset.Waves,
        ntpTime.getTimestamp()
      )
      .explicitGet()

  private def leaseCancelTransaction(sender: KeyPair, leaseId: ByteStr) =
    LeaseCancelTransaction.selfSigned(TxVersion.V3, sender, leaseId, 0.001.waves, ntpTime.getTimestamp()).explicitGet()

  private val genesisWithLease = for {
    sender  <- accountGen
    genesis <- genesisGeneratorP(sender.toAddress)
    leaseTx <- leaseGen(sender, ntpTime.correctedTime())
  } yield (sender, genesis, leaseTx)

  private val genesisWithSetScriptAndInvoke = for {
    sender    <- accountGen
    genesis   <- genesisGeneratorP(sender.toAddress)
    recipient <- accountGen
  } yield (
    sender,
    genesis,
    setScriptTransaction(sender),
    InvokeScriptTransaction
      .selfSigned(
        TxVersion.V2,
        sender,
        sender.toAddress,
        Some(
          FUNCTION_CALL(
            FunctionHeader.User("leaseTo"),
            List(CONST_BYTESTR(ByteStr(recipient.toAddress.bytes)).explicitGet(), CONST_LONG(10_000.waves))
          )
        ),
        Seq.empty,
        0.005.waves,
        Asset.Waves,
        ntpTime.getTimestamp()
      )
      .explicitGet(),
    recipient.toAddress
  )

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

  "returns active leases which were" - {
    "created and cancelled by Lease/LeaseCancel transactions" in forAll(genesisWithLease) {
      case (sender, genesis, leaseTransaction) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis)
          d.appendBlock(leaseTransaction)
          val expectedDetails = Seq(leaseTransaction.id() -> toDetails(leaseTransaction))
          // check liquid block
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)

          d.appendMicroBlock(leaseCancelTransaction(sender, leaseTransaction.id()))
          // check liquid block
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
        }
    }

    "created by LeaseTransaction and canceled by InvokeScriptTransaction" in forAll(genesisWithLease) {
      case (sender, genesis, leaseTransaction) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis)
          d.appendBlock(leaseTransaction)
          val expectedDetails = Seq(leaseTransaction.id() -> toDetails(leaseTransaction))
          // check liquid block
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)

          d.appendMicroBlock(
            setScriptTransaction(sender),
            invokeLeaseCancel(sender, leaseTransaction.id())
          )
          // check liquid block
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
        }
    }

    "created by InvokeScriptTransaction and canceled by CancelLeaseTransaction" in forAll(genesisWithSetScriptAndInvoke) {
      case (sender, genesis, setScript, invoke, recipient) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis, setScript)
          d.appendBlock(invoke)
          val leaseId = d.blockchain
            .accountData(genesis.recipient, "leaseId")
            .collect {
              case i: BinaryDataEntry => i.value
            }
            .get
          val expectedDetails = Seq(leaseId -> LeaseDetails(setScript.sender, recipient, invoke.id(), 10_000.waves, true))
          // check liquid block
          checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)

          d.appendMicroBlock(leaseCancelTransaction(sender, leaseId))
          // check liquid block
          checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(recipient, r, Seq.empty)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(recipient, r, Seq.empty)
        }
    }
    "created and canceled by InvokeScriptTransaction" in forAll(genesisWithSetScriptAndInvoke) {
      case (sender, genesis, setScript, invoke, recipient) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis, setScript)
          d.appendBlock(invoke)
          val leaseId = d.blockchain
            .accountData(genesis.recipient, "leaseId")
            .collect {
              case i: BinaryDataEntry => i.value
            }
            .get
          val expectedDetails = Seq(leaseId -> LeaseDetails(setScript.sender, recipient, invoke.id(), 10_000.waves, true))
          // check liquid block
          checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)

          d.appendMicroBlock(invokeLeaseCancel(sender, leaseId))
          // check liquid block
          checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(recipient, r, Seq.empty)
          // check hardened block
          d.appendKeyBlock()
          checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(recipient, r, Seq.empty)
        }
    }
  }
}
