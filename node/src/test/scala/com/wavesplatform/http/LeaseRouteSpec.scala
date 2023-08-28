package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity}
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.api.common.{CommonAccountsApi, LeaseInfo}
import com.wavesplatform.api.http.RouteTimeout
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.{V5, V6}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{BinaryDataEntry, Blockchain, Height, TxMeta}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.TxHelpers.{defaultSigner, secondSigner, signer}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Asset, Authorized, EthTxGenerator, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.{SharedSchedulerMixin, SystemTime}
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{NTPTime, TestWallet, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.concurrent.Future
import scala.concurrent.duration.*

class LeaseRouteSpec
    extends RouteSpec("/leasing")
    with TransactionGen
    with RestAPISettingsHelper
    with NTPTime
    with WithDomain
    with TestWallet
    with PathMockFactory
    with SharedSchedulerMixin {
  private def route(domain: Domain) =
    LeaseApiRoute(
      restAPISettings,
      testWallet,
      domain.blockchain,
      (_, _) => Future.successful(TracedResult(Right(true))),
      ntpTime,
      CommonAccountsApi(() => domain.blockchainUpdater.snapshotBlockchain, domain.rdb, domain.blockchain),
      new RouteTimeout(60.seconds)(sharedScheduler)
    )

  private def withRoute(balances: Seq[AddrWithBalance], settings: WavesSettings = mostRecent)(f: (Domain, Route) => Unit): Unit =
    withDomain(settings = settings, balances = balances) { d =>
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
    Signed.invokeScript(
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

  private def leaseCancelTransaction(sender: KeyPair, leaseId: ByteStr) =
    LeaseCancelTransaction.selfSigned(TxVersion.V3, sender, leaseId, 0.001.waves, ntpTime.getTimestamp()).explicitGet()

  private def checkDetails(id: ByteStr, details: LeaseDetails, json: JsObject): Unit = {
    (json \ "id").as[ByteStr] shouldEqual id
    (json \ "originTransactionId").as[ByteStr] shouldEqual details.sourceId
    (json \ "sender").as[String] shouldEqual details.sender.toAddress.toString
    (json \ "amount").as[Long] shouldEqual details.amount
  }

  private def checkActiveLeasesFor(address: AddressOrAlias, route: Route, expectedDetails: Seq[(ByteStr, LeaseDetails)]): Unit =
    Get(routePath(s"/active/$address")) ~> route ~> check {
      val resp = responseAs[Seq[JsObject]]
      resp.size shouldEqual expectedDetails.size
      resp.zip(expectedDetails).foreach { case (json, (id, details)) =>
        checkDetails(id, details, json)
      }
    }

  private def toDetails(lt: LeaseTransaction) = LeaseDetails(lt.sender, lt.recipient, lt.amount.value, LeaseDetails.Status.Active, lt.id(), 1)

  private def leaseGen(sender: KeyPair, maxAmount: Long, timestamp: Long): Gen[LeaseTransaction] =
    for {
      fee       <- smallFeeGen
      recipient <- accountGen
      amount    <- Gen.chooseNum(1, (maxAmount - fee).max(1))
      version   <- Gen.oneOf(1.toByte, 2.toByte, 3.toByte)
    } yield LeaseTransaction.selfSigned(version, sender, recipient.toAddress, amount, fee, timestamp).explicitGet()

  "returns active leases which were" - {
    val sender  = TxHelpers.signer(1)
    val leaseTx = leaseGen(sender, ENOUGH_AMT, ntpTime.correctedTime())

    "created and cancelled by Lease/LeaseCancel transactions" in forAll(leaseTx) { leaseTransaction =>
      withRoute(Seq(AddrWithBalance(sender.toAddress))) { (d, r) =>
        d.appendBlock(leaseTransaction)
        val expectedDetails = Seq(leaseTransaction.id() -> toDetails(leaseTransaction))
        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)
        }

        d.appendMicroBlock(leaseCancelTransaction(sender, leaseTransaction.id()))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
        }
      }
    }

    "created by LeaseTransaction and canceled by InvokeScriptTransaction" in forAll(leaseTx) { leaseTransaction =>
      withRoute(Seq(AddrWithBalance(sender.toAddress))) { (d, r) =>
        d.appendBlock(leaseTransaction)
        val expectedDetails = Seq(leaseTransaction.id() -> toDetails(leaseTransaction))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)
        }

        d.appendMicroBlock(
          setScriptTransaction(sender),
          invokeLeaseCancel(sender, leaseTransaction.id())
        )

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
        }
      }
    }

    val setScriptAndInvoke = {
      val sender    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)

      (
        sender,
        setScriptTransaction(sender),
        Signed
          .invokeScript(
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
          ),
        recipient.toAddress
      )
    }

    "created by InvokeScriptTransaction and canceled by CancelLeaseTransaction" in forAll(setScriptAndInvoke) {
      case (sender, setScript, invoke, recipient) =>
        withRoute(Seq(AddrWithBalance(sender.toAddress))) { (d, r) =>
          d.appendBlock(setScript)
          d.appendBlock(invoke)
          val leaseId = d.blockchain
            .accountData(sender.toAddress, "leaseId")
            .collect { case i: BinaryDataEntry =>
              i.value
            }
            .get
          val expectedDetails = Seq(leaseId -> LeaseDetails(setScript.sender, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
            checkActiveLeasesFor(recipient, r, expectedDetails)
          }

          d.appendMicroBlock(leaseCancelTransaction(sender, leaseId))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
            checkActiveLeasesFor(recipient, r, Seq.empty)
          }
        }
    }

    "created and canceled by InvokeScriptTransaction" in forAll(setScriptAndInvoke) { case (sender, setScript, invoke, recipient) =>
      withRoute(Seq(AddrWithBalance(sender.toAddress))) { (d, r) =>
        d.appendBlock(setScript)
        d.appendBlock(invoke)
        val invokeStatus = d.blockchain.transactionMeta(invoke.id()).get.status
        assert(invokeStatus == Status.Succeeded, "Invoke has failed")

        val leaseId = d.blockchain
          .accountData(sender.toAddress, "leaseId")
          .collect { case i: BinaryDataEntry =>
            i.value
          }
          .get
        val expectedDetails = Seq(leaseId -> LeaseDetails(setScript.sender, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)
        }

        d.appendMicroBlock(invokeLeaseCancel(sender, leaseId))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(recipient, r, Seq.empty)
        }
      }
    }

    val invokeExpression = for {
      sender    <- accountGen
      recipient <- accountGen
      invokeExp <- invokeExpressionTransactionGen(
        sender,
        TestCompiler(V6).compileFreeCall(
          s"""
             |let lease = Lease(Address(base58'${recipient.toAddress.toString}'), ${10000.waves})
             |[
             |  lease,
             |  BinaryEntry("leaseId", lease.calculateLeaseId())
             |]""".stripMargin
        ),
        0.01.waves
      )
    } yield (
      sender,
      invokeExp,
      recipient.toAddress
    )

    "created by InvokeExpressionTransaction and canceled by CancelLeaseTransaction" in forAll(invokeExpression) { case (sender, invoke, recipient) =>
      withRoute(AddrWithBalance.enoughBalances(sender), DomainPresets.ContinuationTransaction) { (d, r) =>
        d.appendBlock(invoke)
        val leaseId = d.blockchain
          .accountData(sender.toAddress, "leaseId")
          .collect { case i: BinaryDataEntry =>
            i.value
          }
          .get
        val expectedDetails = Seq(leaseId -> LeaseDetails(sender.publicKey, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)
        }

        d.appendMicroBlock(leaseCancelTransaction(sender, leaseId))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(recipient, r, Seq.empty)
        }
      }
    }

    "created by EthereumTransaction and canceled by CancelLeaseTransaction" in {
      val sender    = signer(2).toEthKeyPair
      val dApp      = defaultSigner
      val recipient = secondSigner.toAddress
      val invoke =
        EthTxGenerator.generateEthInvoke(
          keyPair = sender,
          address = dApp.toAddress,
          funcName = "leaseTo",
          args = Seq(Arg.Bytes(ByteStr(recipient.bytes)), Arg.Integer(10000.waves)),
          payments = Seq.empty
        )
      withRoute(Seq(AddrWithBalance(dApp.toAddress), AddrWithBalance(invoke.sender.toAddress))) { (d, r) =>
        d.appendBlock(setScriptTransaction(dApp))
        d.appendBlock(invoke)
        val leaseId = d.blockchain
          .accountData(dApp.toAddress, "leaseId")
          .collect { case i: BinaryDataEntry =>
            i.value
          }
          .get
        val expectedDetails = Seq(leaseId -> LeaseDetails(dApp.publicKey, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(dApp.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)
        }

        d.appendMicroBlock(leaseCancelTransaction(dApp, leaseId))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(dApp.toAddress, r, Seq.empty)
          checkActiveLeasesFor(recipient, r, Seq.empty)
        }
      }
    }

    val nestedInvocation = {
      val proxy     = TxHelpers.signer(1)
      val target    = TxHelpers.signer(2)
      val recipient = TxHelpers.signer(3)

      (
        (proxy, target, recipient.toAddress),
        Seq(
          setScriptTransaction(target),
          SetScriptTransaction
            .selfSigned(
              TxVersion.V2,
              proxy,
              Some(TestCompiler(V5).compileContract("""
                                                      |{-# STDLIB_VERSION 4 #-}
                                                      |{-# CONTENT_TYPE DAPP #-}
                                                      |{-# SCRIPT_TYPE ACCOUNT #-}
                                                      |
                                                      |@Callable(inv)
                                                      |func callProxy(targetDapp: ByteVector, recipient: ByteVector, amount: Int) = {
                                                      |  strict result = invoke(Address(targetDapp), "leaseTo", [recipient, amount], [])
                                                      |  []
                                                      |}
                                                      |""".stripMargin)),
              0.01.waves,
              ntpTime.getTimestamp()
            )
            .explicitGet()
        )
      )
    }

    "created by nested invocations" in {
      val ((proxy, target, recipient), transactions) = nestedInvocation
      withRoute(Seq(AddrWithBalance(proxy.toAddress), AddrWithBalance(target.toAddress))) { (d, r) =>
        d.appendBlock(transactions*)
        val ist = Signed
          .invokeScript(
            TxVersion.V2,
            proxy,
            proxy.toAddress,
            Some(
              FUNCTION_CALL(
                FunctionHeader.User("callProxy"),
                List(
                  CONST_BYTESTR(ByteStr(target.toAddress.bytes)).explicitGet(),
                  CONST_BYTESTR(ByteStr(recipient.bytes)).explicitGet(),
                  CONST_LONG(10_000.waves)
                )
              )
            ),
            Seq.empty,
            0.005.waves,
            Asset.Waves,
            ntpTime.getTimestamp()
          )

        d.appendBlock(ist)
        val leaseId = d.blockchain
          .accountData(target.toAddress, "leaseId")
          .collect { case i: BinaryDataEntry =>
            i.value
          }
          .get

        val expectedDetails = Seq(leaseId -> LeaseDetails(target.publicKey, recipient, 10_000.waves, LeaseDetails.Status.Active, ist.id(), 1))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(target.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)
        }
      }
    }
  }

  "returns leases created by invoke only for lease sender or recipient" in {
    val invoker         = TxHelpers.signer(1)
    val dApp1           = TxHelpers.signer(2)
    val dApp2           = TxHelpers.signer(3)
    val leaseRecipient1 = TxHelpers.signer(4)
    val leaseRecipient2 = TxHelpers.signer(5)

    val leaseAmount1 = 1
    val leaseAmount2 = 2

    val dAppScript1 = TestCompiler(V5)
      .compileContract(s"""
                          |{-# STDLIB_VERSION 5 #-}
                          |{-# CONTENT_TYPE DAPP #-}
                          |{-# SCRIPT_TYPE ACCOUNT #-}
                          |
                          |@Callable(i)
                          |func foo() = {
                          |  strict inv = invoke(Address(base58'${dApp2.toAddress}'), "bar", [], [])
                          |  let lease = Lease(Address(base58'${leaseRecipient1.toAddress}'), 1)
                          |  [lease, BinaryEntry("leaseId", lease.calculateLeaseId())]
                          |}
                          |""".stripMargin)

    val dAppScript2 = TestCompiler(V5)
      .compileContract(s"""
                          |{-# STDLIB_VERSION 5 #-}
                          |{-# CONTENT_TYPE DAPP #-}
                          |{-# SCRIPT_TYPE ACCOUNT #-}
                          |
                          |@Callable(i)
                          |func bar() = {
                          |  let lease = Lease(Address(base58'${leaseRecipient2.toAddress}'), 2)
                          |  [lease, BinaryEntry("leaseId", lease.calculateLeaseId())]
                          |}
                          |""".stripMargin)

    def checkForInvoke(invokeTx: Transaction & Authorized): Unit =
      withRoute(AddrWithBalance.enoughBalances(dApp1, dApp2) :+ AddrWithBalance(invokeTx.sender.toAddress)) { case (d, r) =>
        def getLeaseId(address: Address) =
          d.blockchain
            .accountData(address, "leaseId")
            .collect { case i: BinaryDataEntry =>
              i.value
            }
            .get

        d.appendBlock(
          TxHelpers.setScript(dApp1, dAppScript1),
          TxHelpers.setScript(dApp2, dAppScript2)
        )

        d.appendBlock(invokeTx)

        val leaseDetails1 = Seq(
          getLeaseId(dApp1.toAddress) -> LeaseDetails(
            dApp1.publicKey,
            leaseRecipient1.toAddress,
            leaseAmount1,
            LeaseDetails.Status.Active,
            invokeTx.id(),
            3
          )
        )
        val leaseDetails2 = Seq(
          getLeaseId(dApp2.toAddress) -> LeaseDetails(
            dApp2.publicKey,
            leaseRecipient2.toAddress,
            leaseAmount2,
            LeaseDetails.Status.Active,
            invokeTx.id(),
            3
          )
        )

        checkActiveLeasesFor(invokeTx.sender.toAddress, r, Seq.empty)
        checkActiveLeasesFor(dApp1.toAddress, r, leaseDetails1)
        checkActiveLeasesFor(dApp2.toAddress, r, leaseDetails2)
        checkActiveLeasesFor(leaseRecipient1.toAddress, r, leaseDetails1)
        checkActiveLeasesFor(leaseRecipient2.toAddress, r, leaseDetails2)
      }

    checkForInvoke(TxHelpers.invoke(dApp1.toAddress, Some("foo"), invoker = invoker))
    checkForInvoke(EthTxGenerator.generateEthInvoke(invoker.toEthKeyPair, dApp1.toAddress, "foo", Seq.empty, Seq.empty))
  }

  routePath("/info") in {
    val blockchain = stub[Blockchain]
    val commonApi  = stub[CommonAccountsApi]

    val route = LeaseApiRoute(
      restAPISettings,
      stub[Wallet],
      blockchain,
      stub[TransactionPublisher],
      SystemTime,
      commonApi,
      new RouteTimeout(60.seconds)(sharedScheduler)
    ).route

    val lease       = TxHelpers.lease()
    val leaseCancel = TxHelpers.leaseCancel(lease.id())
    (blockchain.transactionInfo _).when(lease.id()).returning(Some(TxMeta(Height(1), Status.Succeeded, 0L) -> lease))
    (commonApi.leaseInfo _)
      .when(lease.id())
      .returning(
        Some(
          LeaseInfo(
            lease.id(),
            lease.id(),
            lease.sender.toAddress,
            lease.recipient.asInstanceOf[Address],
            lease.amount.value,
            1,
            LeaseInfo.Status.Canceled,
            Some(2),
            Some(leaseCancel.id())
          )
        )
      )
    (commonApi.leaseInfo _).when(*).returning(None)

    Get(routePath(s"/info/${lease.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response should matchJson(s"""{
                                   |  "id" : "${lease.id()}",
                                   |  "originTransactionId" : "${lease.id()}",
                                   |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                   |  "amount" : 1000000000,
                                   |  "height" : 1,
                                   |  "status" : "canceled",
                                   |  "cancelHeight" : 2,
                                   |  "cancelTransactionId" : "${leaseCancel.id()}"
                                   |}""".stripMargin)
    }

    val leasesListJson = Json.parse(s"""[{
                                       |  "id" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                       |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                       |  "amount" : 1000000000,
                                       |  "height" : 1,
                                       |  "status" : "canceled",
                                       |  "cancelHeight" : 2,
                                       |  "cancelTransactionId" : "${leaseCancel.id()}"
                                       |},
                                       {
                                       |  "id" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                       |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                       |  "amount" : 1000000000,
                                       |  "height" : 1,
                                       |  "status" : "canceled",
                                       |  "cancelHeight" : 2,
                                       |  "cancelTransactionId" : "${leaseCancel.id()}"
                                       |}]""".stripMargin)

    Get(routePath(s"/info?id=${lease.id()}&id=${lease.id()}")) ~> route ~> check {
      val response = responseAs[JsArray]
      response should matchJson(leasesListJson)
    }

    Post(
      routePath(s"/info"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Seq(lease.id().toString, lease.id().toString)).toString())
    ) ~> route ~> check {
      val response = responseAs[JsArray]
      response should matchJson(leasesListJson)
    }

    Post(
      routePath(s"/info"),
      HttpEntity(
        ContentTypes.`application/json`,
        Json.obj("ids" -> (0 to restAPISettings.transactionsByAddressLimit).map(_ => lease.id().toString)).toString()
      )
    ) ~> route ~> check {
      val response = responseAs[JsObject]
      response should matchJson("""{
                                  |  "error" : 10,
                                  |  "message" : "Too big sequence requested: max limit is 10000 entries"
                                  |}""".stripMargin)
    }

    Post(
      routePath(s"/info"),
      FormData("id" -> lease.id().toString, "id" -> lease.id().toString)
    ) ~> route ~> check {
      val response = responseAs[JsArray]
      response should matchJson(leasesListJson)
    }

    Get(routePath(s"/info?id=nonvalid&id=${leaseCancel.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response should matchJson(s"""
                                   |{
                                   |  "error" : 116,
                                   |  "message" : "Request contains invalid IDs. nonvalid, ${leaseCancel.id()}",
                                   |  "ids" : [ "nonvalid", "${leaseCancel.id()}" ]
                                   |}""".stripMargin)
    }
  }
}
